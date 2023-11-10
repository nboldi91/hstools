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
import Control.Lens (_1, (^.))
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe
import Data.Time.Clock
import Database.PostgreSQL.Simple (connectPostgreSQL, close)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as A
import qualified Data.Vector as V
import Control.Concurrent
import Control.Monad.Catch
import System.IO
import qualified Colog.Core as L
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
  defaultConfig <- hsToolsDefaultConfig

  loggerChan <- newChan

  let ioLogger :: L.LogAction IO (L.WithSeverity LspServerLog)
      ioLogger = L.cmap (show . prettyMsg) (L.LogAction $ writeChan loggerChan)
      lspLogger :: L.LogAction (LspM config) (L.WithSeverity LspServerLog)
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
    , LSP.defaultConfig = defaultConfig
    }
  where prettyMsg l = "[" <> viaShow (L.getSeverity l) <> "] " <> pretty (L.getMsg l)

handlers :: Handlers (LspM Config)
handlers = mconcat
  [ notificationHandler SInitialized $ newNotificationThread $ \_ -> runInContext "Initialization" tryToConnectToDB

  , notificationHandler SWorkspaceDidChangeConfiguration $ newNotificationThread $ \_ -> runInContext "ChangeConfiguration" $ do
      cfg <- liftLSP LSP.getConfig
      hasNoConnection <- liftIO $ isEmptyMVar (cfConnection cfg)
      when hasNoConnection tryToConnectToDB

  , requestHandler SShutdown $ newRequestThread $ \_ responder -> do
      cfg <- LSP.getConfig
      maybeConn <- liftIO $ tryReadMVar (cfConnection cfg)
      case maybeConn of
        Just conn -> liftIO $ close conn
        Nothing -> return ()
      responder $ Right Empty
      
  , requestHandler STextDocumentDefinition $ newRequestThread $ \(DefinitionParams (TextDocumentIdentifier uri) pos _ _) responder ->
    handlerCtx "STextDocumentDefinition" $ \conn -> do
      ensureFileLocationRequest uri responder $ \file -> do
        rewrites <- getRewrites file
        case newToOriginalPos rewrites (posToSP pos) of
          Right originalPos -> do
            names <- liftIO $ getMatchingNames conn file (spLine originalPos) (spCol originalPos) (Just True)
            results <- mapM lineToLoc names
            liftLSP $ responder $ Right $ InR $ InL $ LSP.List $ take 1 $ catMaybes results
          Left _ -> liftLSP $ responder $ Right $ InR $ InL $ LSP.List [] -- the source position was not in the compiled source code

  , requestHandler STextDocumentReferences $ newRequestThread $ \(ReferenceParams (TextDocumentIdentifier uri) pos _ _ (ReferenceContext includeDefinition)) responder ->
    handlerCtx "STextDocumentReferences" $ \conn -> do
      ensureFileLocationRequest uri responder $ \file -> do
        rewrites <- getRewrites file
        case newToOriginalPos rewrites (posToSP pos) of
          Right originalPos -> do
            names <- liftIO $ getMatchingNames conn file (spLine originalPos) (spCol originalPos) (if not includeDefinition then Just False else Nothing)
            results <- mapM lineToLoc names
            liftLSP $ responder $ Right $ LSP.List $ catMaybes $ results
          Left _ -> liftLSP $ responder $ Right $ LSP.List [] -- the source position was not in the compiled source code

  , requestHandler STextDocumentHover $ newRequestThread $ \(HoverParams (TextDocumentIdentifier uri) pos _workDone) responder ->
    handlerCtx "STextDocumentHover" $ \conn -> do
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

  -- TODO: cancelling the request that has been started
  , notificationHandler SCancelRequest $ newNotificationThread $ const $ return ()
  -- , notificationHandler SCancelRequest $ \message -> handlerCtx "CancelRequest" $ \_conn -> do
  --     let NotificationMessage _ _ (CancelParams (flattenId -> _token)) = message
  --     return ()

  -- already handled by LSP's withProgress
  , notificationHandler SWindowWorkDoneProgressCancel $ const $ return ()

  , notificationHandler (SCustomMethod "TestNotification") $ newNotificationThread $ \_args -> handlerCtx "TestNotification" $ \_conn -> do
      liftIO $ forM_ [1..10] $ const $ threadDelay 1000000
      sendMessage "TestNotification finished"

  , requestHandler (SCustomMethod "TestRequest") $ newRequestThread $ \_args responder -> handlerCtx "TestRequest" $ \_conn -> do
      liftIO $ forM_ [1..10] $ const $ threadDelay 1000000
      liftLSP $ responder $ Right "Test request response"

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

  , notificationHandler SWorkspaceDidChangeWatchedFiles $ newNotificationThread $ \(DidChangeWatchedFilesParams (LSP.List fileChanges)) ->
    handlerCtx "FileChange" $ \conn -> do
      let numFiles = fromIntegral $ Prelude.length fileChanges
      purgeRecordsForChangedFilesOnFailure conn $
        LSP.withProgress "Synchronize database with file changes" LSP.Cancellable $ \report ->
          forM_ ([1..] `zip` fileChanges) $ \(i, (FileEvent uri _)) -> do
            ensureFileLocation uri $ \filePath -> do
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
            report $
              ProgressAmount
                (Just $ round $ 100.0 * fromIntegral i / fromIntegral numFiles)
                (Just $ getUri uri <> " (" <> T.pack (show (i :: Int)) <> "/" <> T.pack (show (numFiles :: Int)) <> ")")
  
  , notificationHandler STextDocumentDidChange $ newNotificationThread $ \(DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _version) (LSP.List changes)) ->
    handlerCtx "STextDocumentDidChange" $ \_ -> do
      ensureFileLocation uri $ \filePath -> do
        let goodChanges = catMaybes $ map textDocChangeToSD changes
        liftLSP LSP.getConfig >>= \cf -> liftIO $ updateSavedFileRecords filePath goodChanges (cfFileRecords cf)
        updateFileStatesFor filePath

  , notificationHandler STextDocumentDidSave $ newNotificationThread $ \(DidSaveTextDocumentParams (TextDocumentIdentifier uri) _reason) -> 
    handlerCtx "STextDocumentDidSave" $ \conn -> do
      ensureFileLocation uri $ \filePath -> do
        cfg <- LSP.getConfig
        fileDiffs <- liftIO $ readMVar (cfFileRecords cfg) >>= return . maybe emptyDiffs frDiffs . Map.lookup filePath
        currentTime <- liftIO getCurrentTime
        let serializedDiff = serializeSourceDiffs fileDiffs
        void $ liftIO $ updateFileDiffs conn filePath currentTime (Just serializedDiff)
  
  , notificationHandler STextDocumentDidOpen $ newNotificationThread $ \(DidOpenTextDocumentParams (TextDocumentItem uri _langId _version content)) ->
    handlerCtx "STextDocumentDidOpen" $ \conn -> do
      ensureFileLocation uri $ \filePath ->
        liftLSP LSP.getConfig >>= \cf -> liftIO $ do
          diffs <- checkIfFileHaveBeenChanged conn filePath
          recordFileOpened conn filePath content diffs (cfFileRecords cf)
  
  , notificationHandler STextDocumentDidClose $ newNotificationThread $ \(DidCloseTextDocumentParams (TextDocumentIdentifier uri)) ->
    handlerCtx "STextDocumentDidClose" $ \conn -> do
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
      -- set up connection
      let dbConn = DbConn (cfLogOptions config) conn
      liftIO $ reinitializeTablesIfNeeded dbConn
      liftIO $ putMVar (cfConnection config) conn
      -- put up listener for module clean
      env <- getLspEnv
      liftIO $ listenToModuleClean dbConn
      -- FIXME: should also use lifted lsp
      void $ liftIO $ forkIO (handleNotifications conn (cfFileRecords config) (resSendMessage env))
      -- check file statuses
      purgeRecordsForChangedFilesOnFailure dbConn $
        LSP.withProgress "Check file status" LSP.Cancellable $ \report -> do 
          modifiedDiffs <- checkIfFilesHaveBeenChanged dbConn report
          let fileRecords = map (\(fp, diff) -> (fp, FileRecord diff)) modifiedDiffs
          liftIO $ putMVar (cfFileRecords config) $ Map.fromList fileRecords
          updateFileStates

    Left (_ :: SomeException) -> return () -- error is OK

checkIfFilesHaveBeenChanged :: DbConn -> (ProgressAmount -> LspMonad ()) -> LspMonad [(FilePath, SourceDiffs Original Modified)]
checkIfFilesHaveBeenChanged conn report = do
  diffs <- liftIO $ getAllModifiedFileDiffs conn
  let numFiles = fromIntegral $ Prelude.length diffs
  forM ([0..] `zip` diffs) $ \(i, diff@(fp, _, _)) -> do
    res <- liftIO $ checkFileHaveBeenChanged conn diff
    report $
      ProgressAmount
        (Just $ round $ 100.0 * fromIntegral i / fromIntegral numFiles)
        (Just $ T.pack fp <> " (" <> T.pack (show (i :: Int)) <> "/" <> T.pack (show (numFiles :: Int)) <> ")")
    return res

purgeRecordsForChangedFilesOnFailure :: DbConn -> LspMonad () -> LspMonad ()
purgeRecordsForChangedFilesOnFailure conn action =
  action
    `catch` \(_ :: LSP.ProgressCancelledException) -> purgeAndNotify
    `catch` \e -> purgeAndNotify >> throwM (e :: SomeException)
  where
    purgeAndNotify = do
      sendError "Checking file status failed, some files will be removed from the DB"
      purgeRecordsForChangedFiles conn

purgeRecordsForChangedFiles :: DbConn -> LspMonad ()
purgeRecordsForChangedFiles conn = do
  diffs <- liftIO $ getAllModifiedFileDiffs conn
  liftIO $ forM_ diffs $ \diff -> do
    modificationTime <- getFileLaterModificationTime diff
    when (isJust modificationTime) $ cleanModuleFromDB conn $ diff ^. _1
  updateFileStates

hstoolsOptions :: Options
hstoolsOptions = defaultOptions
  { executeCommandCommands = Just ["CleanDB", "TestNotification", "TestRequest"]
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
