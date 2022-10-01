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
import Control.Monad.IO.Class
import Control.Monad
import Control.Exception
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing, catMaybes)
import Data.Time.Clock
import Database.PostgreSQL.Simple (connectPostgreSQL)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as A
import qualified Data.Vector as V
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import System.IO
import Colog.Core.Action

import Language.Haskell.HsTools.LspServer.State
import Language.Haskell.HsTools.LspServer.FileRecords
import Language.Haskell.HsTools.LspServer.Monad
import Language.Haskell.HsTools.LspServer.Notifications
import Language.Haskell.HsTools.LspServer.Utils
import Language.Haskell.HsTools.LinesDiff
import Language.Haskell.HsTools.HandleErrors
import Language.Haskell.HsTools.Database

mainWithHandles :: Handle -> Handle -> IO Int
mainWithHandles input output = do
  fileStore <- newEmptyMVar
  runServerWithHandles (LogAction $ const $ return ()) (LogAction $ const $ return ()) input output $ ServerDefinition
    { onConfigurationChange = loadConfig
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \env -> Iso (runLspT env) (liftIO)
    , options = hstoolsOptions
    , LSP.defaultConfig = hsToolsDefaultConfig fileStore
    }

handlers :: Handlers (LspM Config)
handlers = mconcat
  [ notificationHandler SInitialized $ \_ -> runInContext "Initialization" tryToConnectToDB
  , notificationHandler SWorkspaceDidChangeConfiguration $ \_ -> runInContext "ChangeConfiguration" $ do
      cfg <- liftLSP LSP.getConfig
      when (isNothing (cfConnection cfg))
        tryToConnectToDB
      
  , requestHandler STextDocumentDefinition $ \req responder -> handlerCtx "STextDocumentDefinition" $ \conn -> do
      let RequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier uri) pos _ _) = req
      ensureFileLocationRequest uri responder $ \file -> do
        rewrites <- getRewrites file
        case newToOriginalPos rewrites (posToSP pos) of
          Right originalPos -> do
            names <- liftIO $ getMatchingNames conn file (spLine originalPos) (spCol originalPos) (Just True)
            liftLSP $ responder $ Right $ InR $ InL $ LSP.List $ take 1 $ catMaybes $ map (lineToLoc rewrites) names
          Left _ -> liftLSP $ responder $ Right $ InR $ InL $ LSP.List [] -- the source position was not in the compiled source code

  , requestHandler STextDocumentReferences $ \req responder -> handlerCtx "STextDocumentReferences" $ \conn -> do
      let RequestMessage _ _ _ (ReferenceParams (TextDocumentIdentifier uri) pos _ _ (ReferenceContext includeDefinition)) = req
      ensureFileLocationRequest uri responder $ \file -> do
        rewrites <- getRewrites file
        case newToOriginalPos rewrites (posToSP pos) of
          Right originalPos -> do
            names <- liftIO $ getMatchingNames conn file (spLine originalPos) (spCol originalPos) (if not includeDefinition then Just False else Nothing)
            liftLSP $ responder $ Right $ LSP.List $ catMaybes $ map (lineToLoc rewrites) names
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
              (typ, isDefined, name, startLine, startColumn, endLine, endColumn):_ -> 
                let ms = HoverContents $ markedUpContent "hstools" $ T.pack
                            $ name ++ (if isDefined == True then " defined here" else "")
                                ++ (maybe "" ("\n  :: " ++) typ)
                    origRange = SourceRange (SP startLine startColumn) (SP endLine endColumn)
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
              updatedSource <- liftIO $ readFile filePath
              let fileDiffs = sourceDiffs startSP source updatedSource
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
        fileDiffs <- liftIO $ readMVar (cfFileRecords cfg) >>= return . maybe Map.empty frDiffs . Map.lookup filePath
        currentTime <- liftIO getCurrentTime
        let serializedDiff = serializeSourceDiffs fileDiffs
        void $ liftIO $ updateFileDiffs conn filePath currentTime (Just serializedDiff)
  
  , notificationHandler STextDocumentDidOpen $ \msg -> handlerCtx "STextDocumentDidOpen" $ \conn -> do
      let NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _langId _version content)) = msg
      ensureFileLocation uri $ \filePath ->
        liftLSP LSP.getConfig >>= \cf -> liftIO $ recordFileOpened conn filePath content (cfFileRecords cf)
  
  , notificationHandler STextDocumentDidClose $ \msg -> handlerCtx "STextDocumentDidClose" $ \conn -> do
      let NotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri)) = msg
      ensureFileLocation uri $ \filePath ->
        liftLSP LSP.getConfig >>= \cf -> liftIO $ recordFileClosed conn filePath (cfFileRecords cf)
  ]

handlerCtx ctx action = runInContext ctx $ withConnection $ \conn -> handleErrorsCtx conn $ action conn

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

sendFileStates :: [(FilePath, SourceDiffs)] -> LspMonad ()
sendFileStates [] = return () 
sendFileStates states 
  = liftLSP $ sendNotification changeFileStatesMethod $ createChangeFileStates states

tryToConnectToDB :: LspMonad ()
tryToConnectToDB = do
  config <- liftLSP LSP.getConfig
  connOrError <- liftIO $ try $ connectPostgreSQL (BS.pack (cfPostgresqlConnectionString config))
  case connOrError of
    Right conn -> handleErrors conn "tryToConnectToDB" $ do 
      sendMessage "Connected to DB"
      liftIO $ reinitializeTablesIfNeeded conn
      modifiedDiffs <- liftIO $ getAllModifiedFileDiffs conn 
      let fileRecords = map (\(fp, diff) -> (fp, FileRecord $ fromMaybe Map.empty $ fmap deserializeSourceDiffs diff)) modifiedDiffs
      liftIO $ putMVar (cfFileRecords config) $ Map.fromList fileRecords
      liftLSP $ LSP.setConfig $ config { cfConnection = Just conn }
      updateFileStates
      env <- getLspEnv
      liftIO $ listenToModuleClean conn
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
