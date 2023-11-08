{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.HsTools.LspServer.LspServer where

import Language.LSP.Server as LSP
import Language.LSP.Types as LSP
import Language.LSP.Logging as LSP
import Control.Monad.IO.Class
import Control.Monad
import Control.Exception
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (isNothing, catMaybes)
import Data.Time.Clock
import Database.PostgreSQL.Simple (connectPostgreSQL, close)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as A
import qualified Data.Vector as V
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import System.IO
import qualified Colog.Core as L
import Colog.Core (LogAction (..), WithSeverity (..))
import Prettyprinter

import Language.Haskell.HsTools.LspServer.State
import Language.Haskell.HsTools.LspServer.FileRecords
import Language.Haskell.HsTools.LspServer.Monad
import Language.Haskell.HsTools.LspServer.Notifications
import Language.Haskell.HsTools.LspServer.Utils
import Language.Haskell.HsTools.SourceDiffs
import Language.Haskell.HsTools.SourcePosition as SP
import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.Utils

mainWithHandles :: Handle -> Handle -> IO Int
mainWithHandles input output = do
  fileStore <- newEmptyMVar

  loggerChan <- newChan

  let ioLogger :: LogAction IO (WithSeverity LspServerLog)
      ioLogger = L.cmap (show . prettyMsg) (L.LogAction $ writeChan loggerChan)
      lspLogger :: LogAction (LspM config) (WithSeverity LspServerLog)
      lspLogger =
        let clientLogger = L.cmap (fmap (T.pack . show . pretty)) LSP.defaultClientLogger
        in clientLogger <> L.hoistLogAction liftIO ioLogger

  forkIO $ forever $ readChan loggerChan >>= \str -> hPutStrLn stderr str >> hFlush stderr

  runServerWithHandles ioLogger lspLogger input output $ ServerDefinition
    { onConfigurationChange = loadConfig
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \env -> Iso (runLspT env) (liftIO)
    , options = hstoolsOptions
    , LSP.defaultConfig = hsToolsDefaultConfig fileStore
    }
  where prettyMsg l = "[" <> viaShow (L.getSeverity l) <> "] " <> pretty (L.getMsg l)

handlers :: Handlers (LspM Config)
handlers = mconcat
  [ notificationHandler SInitialized $ \_ -> runInContext "Initialization" tryToConnectToDB
  , notificationHandler SWorkspaceDidChangeConfiguration $ \_ -> runInContext "ChangeConfiguration" $ do
      cfg <- liftLSP LSP.getConfig
      when (isNothing (cfConnection cfg))
        tryToConnectToDB
  , requestHandler SShutdown $ \_ responder -> do
      cfg <- LSP.getConfig
      case cfConnection cfg of
        Just conn -> liftIO $ close conn
        Nothing -> return ()
      responder $ Right Empty
      
  , requestHandler STextDocumentDefinition $ \req responder -> handlerCtx "STextDocumentDefinition" $ \conn -> do
      let RequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier uri) pos _ _) = req
      ensureFileLocationRequest uri responder $ \file -> do
        rewrites <- getRewrites file
        case newToOriginalPos rewrites (posToSP pos) of
          Right originalPos -> do
            names <- liftIO $ getMatchingNames conn file (spLine originalPos) (spCol originalPos) (Just True)
            results <- mapM lineToLoc names
            liftLSP $ responder $ Right $ InR $ InL $ LSP.List $ take 1 $ catMaybes results
          Left _ -> liftLSP $ responder $ Right $ InR $ InL $ LSP.List [] -- the source position was not in the compiled source code

  , requestHandler STextDocumentReferences $ \req responder -> handlerCtx "STextDocumentReferences" $ \conn -> do
      let RequestMessage _ _ _ (ReferenceParams (TextDocumentIdentifier uri) pos _ _ (ReferenceContext includeDefinition)) = req
      ensureFileLocationRequest uri responder $ \file -> do
        rewrites <- getRewrites file
        case newToOriginalPos rewrites (posToSP pos) of
          Right originalPos -> do
            names <- liftIO $ getMatchingNames conn file (spLine originalPos) (spCol originalPos) (if not includeDefinition then Just False else Nothing)
            results <- mapM lineToLoc names
            liftLSP $ responder $ Right $ LSP.List $ catMaybes $ results
          Left _ -> liftLSP $ responder $ Right $ LSP.List [] -- the source position was not in the compiled source code

  , requestHandler STextDocumentHover $ \req responder -> handlerCtx "STextDocumentHover" $ \conn -> do
      let RequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos _workDone) = req
      ensureFileLocationRequest uri responder $ \file -> do
        rewrites <- getRewrites file
        case newToOriginalPos rewrites (posToSP pos) of
          Right originalPos -> do
            let lineNum = fromIntegral (spLine originalPos)
                columnNum = fromIntegral (spCol originalPos)
            names <- liftIO $ getHoverInfo conn file lineNum columnNum
            case names of
              [] -> liftLSP $ responder (Right Nothing)  
              (typ, isDefined, name, startLine, startColumn, endLine, endColumn, commentText):_ -> 
                let ms = HoverContents $ markedUpContent "hstools" $ T.pack
                            $ name ++ (if isDefined == True then " defined here" else "")
                                ++ (maybe "" ("\n  :: " ++) typ)
                                ++ (maybe "" ("\n" ++) commentText)
                    origRange = SP.Range (SP startLine startColumn) (SP endLine endColumn)
                    newRange = originalToNewRangeStrict rewrites origRange
                    rsp = Hover ms (fmap rangeToLSP newRange)
                in liftLSP $ responder (Right $ Just rsp)  
          Left _ -> liftLSP $ responder (Right Nothing) -- the source position was not in the compiled source code


  , notificationHandler SCancelRequest $ const $ return ()

  , notificationHandler (SCustomMethod "CleanDB") $ \message -> handlerCtx "CleanDB" $ \conn -> do
      let NotificationMessage _ _ args = message
      case args of
        A.Array (V.toList -> [ A.Null ]) -> do
          liftIO $ reinitializeTables conn
          sendMessage "DB cleaned"
        A.Array (V.toList -> [ A.String s ])
          -> sendMessage $ "Cleaning DB for path: " <> s
        _ -> sendError $ T.pack $ "Unrecognized CleanDB argument: " ++ show args
      updateFileStates

  , notificationHandler SWorkspaceDidChangeWatchedFiles $ \message -> handlerCtx "FileChange" $ \conn -> do
      let NotificationMessage _ _ (DidChangeWatchedFilesParams (LSP.List fileChanges)) = message
      forM_ fileChanges $ \(FileEvent uri _) -> ensureFileLocation uri $ \filePath -> do
        isFileOpen <- liftLSP LSP.getConfig >>= liftIO . isFileOpen filePath . cfFileRecords
        unless isFileOpen $ do
          compiledSource <- liftIO $ getCompiledSource conn filePath
          case compiledSource of
            Just source -> do
              updatedSource <- liftIO $ readFileContent filePath
              let fileDiffs = maybe emptyDiffs (createSourceDiffs (SP 1 1) (SP 1 1) source) updatedSource
              liftLSP LSP.getConfig >>= \cf -> liftIO $ replaceSourceDiffs filePath fileDiffs $ cfFileRecords cf
              currentTime <- liftIO getCurrentTime
              let serializedDiff = nothingIfEmpty $ serializeSourceDiffs fileDiffs
              void $ liftIO $ updateFileDiffs conn filePath currentTime serializedDiff
            Nothing -> return () -- the file is not compiled yet, nothing to do
        updateFileStatesFor filePath
  
  , notificationHandler STextDocumentDidChange $ \msg -> handlerCtx "STextDocumentDidChange" $ \_ -> do
      let NotificationMessage _ _ (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _version) (LSP.List changes)) = msg
      ensureFileLocation uri $ \filePath -> do
        let goodChanges = catMaybes $ map textDocChangeToSD changes
        liftLSP LSP.getConfig >>= \cf -> liftIO $ updateSavedFileRecords filePath goodChanges (cfFileRecords cf)
        updateFileStatesFor filePath

  , notificationHandler STextDocumentDidSave $ \msg -> handlerCtx "STextDocumentDidSave" $ \conn -> do
      let NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _reason) = msg
      ensureFileLocation uri $ \filePath -> do
        cfg <- LSP.getConfig
        fileDiffs <- liftIO $ readMVar (cfFileRecords cfg) >>= return . maybe emptyDiffs frDiffs . Map.lookup filePath
        currentTime <- liftIO getCurrentTime
        let serializedDiff = serializeSourceDiffs fileDiffs
        void $ liftIO $ updateFileDiffs conn filePath currentTime (Just serializedDiff)
  
  , notificationHandler STextDocumentDidOpen $ \msg -> handlerCtx "STextDocumentDidOpen" $ \conn -> do
      let NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _langId _version content)) = msg
      ensureFileLocation uri $ \filePath ->
        liftLSP LSP.getConfig >>= \cf -> liftIO $ do
          diffs <- checkIfFileHaveBeenChanged conn filePath
          recordFileOpened conn filePath content diffs (cfFileRecords cf)
  
  , notificationHandler STextDocumentDidClose $ \msg -> handlerCtx "STextDocumentDidClose" $ \conn -> do
      let NotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri)) = msg
      ensureFileLocation uri $ \filePath ->
        liftLSP LSP.getConfig >>= \cf -> liftIO $ recordFileClosed conn filePath (cfFileRecords cf)
  ]

handlerCtx ctx action = runInContext ctx $ withConnection $ \conn -> handleErrorsCtx (dbConnConnection conn) $ action conn

updateFileStates :: LspMonad ()
updateFileStates = do
  cfg <- LSP.getConfig
  fr <- liftIO $ readMVar $ cfFileRecords cfg
  sendFileStates $ Map.toList $ Map.map frDiffs fr

updateFileStatesFor :: FilePath -> LspMonad ()
updateFileStatesFor filePath = do
  cfg <- LSP.getConfig
  fr <- liftIO $ readMVar $ cfFileRecords cfg
  let status = Map.lookup filePath $ Map.map frDiffs fr
  sendFileStates $ maybe [] ((:[]) . (filePath,)) status

sendFileStates :: [(FilePath, SourceDiffs Original Modified)] -> LspMonad ()
sendFileStates [] = return () 
sendFileStates states 
  = liftLSP $ sendNotification changeFileStatesMethod $ createChangeFileStates states


tryToConnectToDB :: LspMonad ()
tryToConnectToDB = do
  -- DO NOT LOG HERE, since the configuration might not be configured properly and logging to stdout breaks
  -- the communication with the editor
  config <- liftLSP LSP.getConfig
  connOrError <- liftIO $ try $ connectPostgreSQL (BS.pack (cfPostgresqlConnectionString config))
  case connOrError of
    Right conn -> lspHandleErrors conn "tryToConnectToDB" $ do 
      let dbConn = DbConn (cfLogOptions config) conn
      liftIO $ reinitializeTablesIfNeeded dbConn
      modifiedDiffs <- liftIO $ checkIfFilesHaveBeenChanged dbConn
      let fileRecords = map (\(fp, diff) -> (fp, FileRecord diff)) modifiedDiffs
      liftIO $ putMVar (cfFileRecords config) $ Map.fromList fileRecords
      liftLSP $ LSP.setConfig $ config { cfConnection = Just conn }
      updateFileStates
      env <- getLspEnv
      liftIO $ listenToModuleClean dbConn
      void $ liftIO $ forkIO (handleNotifications conn (cfFileRecords config) (resSendMessage env))
    Left (_ :: SomeException) -> return () -- error is OK

hstoolsOptions :: Options
hstoolsOptions = defaultOptions
  { executeCommandCommands = Just ["cleanDB"]
  , textDocumentSync = Just syncOptions
  }
  where
    syncOptions =
      TextDocumentSyncOptions 
        (Just True) -- openClose notification
        (Just TdSyncIncremental) -- change notification
        Nothing -- willSave notification
        Nothing -- willSave request
        (Just $ InL True) -- save notification
