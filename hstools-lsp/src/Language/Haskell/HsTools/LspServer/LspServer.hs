{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.HsTools.LspServer.LspServer where

import Language.LSP.Server as LSP
import Language.LSP.Types as LSP
import Language.LSP.Logging as LSP
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Lens (_1, (^.))
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Maybe
import Data.Time.Clock
import Database.PostgreSQL.Simple (connectPostgreSQL, close)
import qualified Database.PostgreSQL.Simple.Notification as SQL
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
  [ notification SInitialized $ const tryToConnectToDB

  -- the configuration change is already performed by loadConfig
  , notification SWorkspaceDidChangeConfiguration $ \DidChangeConfigurationParams{} -> do
      cfg <- liftLSP LSP.getConfig
      hasNoConnection <- liftIO $ isEmptyMVar (cfConnection cfg)
      when hasNoConnection tryToConnectToDB

  , request SShutdown $ \Empty responder -> do
      cfg <- LSP.getConfig
      maybeConn <- liftIO $ tryReadMVar (cfConnection cfg)
      case maybeConn of
        Just conn -> liftIO $ close conn
        Nothing -> return ()
      liftLSP $ responder $ Right Empty
      
  , request STextDocumentDefinition $ \(DefinitionParams (TextDocumentIdentifier uri) pos _ _) responder ->
      ensureFileLocationRequest uri responder $ \file -> do
        rewrites <- getRewrites file
        case newToOriginalPos rewrites (posToSP pos) of
          Right originalPos -> do
            names <- getMatchingNames file (spLine originalPos) (spCol originalPos) (Just True)
            results <- mapM lineToLoc names
            liftLSP $ responder $ Right $ InR $ InL $ LSP.List $ take 1 $ catMaybes results
          Left _ -> liftLSP $ responder $ Right $ InR $ InL $ LSP.List [] -- the source position was not in the compiled source code

  , request STextDocumentReferences $ \(ReferenceParams (TextDocumentIdentifier uri) pos _ _ (ReferenceContext includeDefinition)) responder ->
      ensureFileLocationRequest uri responder $ \file -> do
        rewrites <- getRewrites file
        case newToOriginalPos rewrites (posToSP pos) of
          Right originalPos -> do
            names <- getMatchingNames file (spLine originalPos) (spCol originalPos) (if not includeDefinition then Just False else Nothing)
            results <- mapM lineToLoc names
            liftLSP $ responder $ Right $ LSP.List $ catMaybes $ results
          Left _ -> liftLSP $ responder $ Right $ LSP.List [] -- the source position was not in the compiled source code

  , request STextDocumentHover $ \(HoverParams (TextDocumentIdentifier uri) pos _workDone) responder ->
      ensureFileLocationRequest uri responder $ \file -> do
        liftIO $ threadDelay 10000000
        rewrites <- getRewrites file
        case newToOriginalPos rewrites (posToSP pos) of
          Right originalPos -> do
            let lineNum = fromIntegral (spLine originalPos)
                columnNum = fromIntegral (spCol originalPos)
            names <- getHoverInfo file lineNum columnNum
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

  , notification SCancelRequest $ \(CancelParams token) -> do
      requestThreads <- cfRequestThreads <$> LSP.getConfig
      -- looks up a request by its token from the map of concurrent requests and kills the thread
      -- the record will automatically be removed from the map by its handler on exception
      threadId <- liftIO $ Map.lookup (LSP.SomeLspId token) <$> readMVar requestThreads
      liftIO $ maybe (return ()) (\tid -> throwTo tid RequestCancelledException) threadId

  -- already handled by LSP's withProgress
  , notification SWindowWorkDoneProgressCancel $ \WorkDoneProgressCancelParams{} -> return ()

  , notification (SCustomMethod "TestNotification") $ \_args -> do
      liftIO $ forM_ [1..10] $ const $ threadDelay 1000000
      sendMessage "TestNotification finished"

  , request (SCustomMethod "TestRequest") $ \_args responder -> do
      liftIO $ forM_ [1..10] $ const $ threadDelay 1000000
      liftLSP $ responder $ Right "Test request response"

  , notification (SCustomMethod "CleanDB") $ \args -> do
      case args of
        A.Array (V.toList -> [ A.Null ]) -> do
          reinitializeTables
          sendMessage "DB cleaned"
        A.Array (V.toList -> [ A.String s ])
          -> sendMessage $ "Cleaning DB for path: " <> s
        _ -> sendError $ T.pack $ "Unrecognized CleanDB argument: " ++ show args
      updateFileStates

  , notification SWorkspaceDidChangeWatchedFiles $ \(DidChangeWatchedFilesParams (LSP.List fileChanges)) ->
      purgeRecordsForChangedFilesOnFailure $ do
        let numFiles = fromIntegral $ Prelude.length fileChanges
        LSP.withProgress "Synchronize database with file changes" LSP.Cancellable $ \report ->
          forM_ ([1..] `zip` fileChanges) $ \(i, (FileEvent uri _)) -> do
            ensureFileLocation uri $ \filePath -> do
              isFileOpen <- liftLSP LSP.getConfig >>= liftIO . isFileOpen filePath . cfFileRecords
              unless isFileOpen $ do
                compiledSource <- getCompiledSource filePath
                case compiledSource of
                  Just source -> do
                    updatedSource <- liftIO $ readFileContent filePath
                    let fileDiffs = maybe emptyDiffs (createSourceDiffs (SP 1 1) (SP 1 1) source) updatedSource
                    liftLSP LSP.getConfig >>= \cf -> liftIO $ replaceSourceDiffs filePath fileDiffs $ cfFileRecords cf
                    currentTime <- liftIO getCurrentTime
                    let serializedDiff = nothingIfEmpty $ serializeSourceDiffs fileDiffs
                    void $ updateFileDiffs filePath currentTime serializedDiff
                  Nothing -> return () -- the file is not compiled yet, nothing to do
              updateFileStatesFor filePath
            report $
              ProgressAmount
                (Just $ round $ 100.0 * fromIntegral i / fromIntegral numFiles)
                (Just $ getUri uri <> " (" <> T.pack (show (i :: Int)) <> "/" <> T.pack (show (numFiles :: Int)) <> ")")
  
  , notification STextDocumentDidChange $ \(DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _version) (LSP.List changes)) ->
      ensureFileLocation uri $ \filePath -> do
        let goodChanges = catMaybes $ map textDocChangeToSD changes
        liftLSP LSP.getConfig >>= liftIO . updateSavedFileRecords filePath goodChanges . cfFileRecords
        updateFileStatesFor filePath

  , notification STextDocumentDidSave $ \(DidSaveTextDocumentParams (TextDocumentIdentifier uri) _reason) -> 
      ensureFileLocation uri $ \filePath -> do
        cfg <- LSP.getConfig
        fileDiffs <- liftIO $ readMVar (cfFileRecords cfg) >>= return . maybe emptyDiffs frDiffs . Map.lookup filePath
        currentTime <- liftIO getCurrentTime
        let serializedDiff = serializeSourceDiffs fileDiffs
        void $ updateFileDiffs filePath currentTime $ Just serializedDiff
  
  , notification STextDocumentDidOpen $ \(DidOpenTextDocumentParams (TextDocumentItem uri _langId _version content)) ->
      ensureFileLocation uri $ \filePath ->
        liftLSP LSP.getConfig >>= \cf -> do
          diffs <- checkIfFileHaveBeenChanged filePath
          recordFileOpened filePath content diffs $ cfFileRecords cf
  
  , notification STextDocumentDidClose $ \(DidCloseTextDocumentParams (TextDocumentIdentifier uri)) ->
      ensureFileLocation uri $ \filePath ->
        liftLSP LSP.getConfig >>= recordFileClosed filePath . cfFileRecords
  ]

notification :: forall (m :: Method FromClient Notification) . SMethod m -> (MessageParams m -> LspMonad ()) -> Handlers (LspT Config IO)
notification method action =
  notificationHandler method $ \msg -> runInContext (show method) $ newNotificationThread msg $ \params -> handleErrorsCtx $ action params

request ::
  forall (m :: Method FromClient Request) .
    SMethod m -> (MessageParams m -> (Either ResponseError (ResponseResult m) -> Lsp ()) -> LspMonad ()) -> Handlers (LspT Config IO)
request method action =
  requestHandler method $ \msg resp -> runInContext (show method) $ newRequestThread msg resp action

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
  config <- liftLSP LSP.getConfig
  connOrError <- liftIO $ try $ connectPostgreSQL (BS.pack (cfPostgresqlConnectionString config))
  case connOrError of
    Right conn -> do
      -- set up connection
      runReaderT reinitializeTablesIfNeeded $ DbConn (cfLogOptions config) conn
      liftIO $ putMVar (cfConnection config) conn
      -- put up listener for module clean
      listenToModuleClean
      runInIO <- askRunInIO
      void $ liftIO $ forkIO $ runInIO handleNotifications
      -- check file statuses
      purgeRecordsForChangedFilesOnFailure $
        LSP.withProgress "Check file status" LSP.Cancellable $ \report -> do 
          maybeModifiedDiffs <- try (checkIfFilesHaveBeenChanged report)
          -- putMVar guarantees that STextDocumentDidOpen will not trigger before the diff info is loaded from the database
          liftIO $ case maybeModifiedDiffs of
            Right modifiedDiffs -> do
              let fileRecords = map (\(fp, diff) -> (fp, FileRecord diff)) modifiedDiffs
              putMVar (cfFileRecords config) $ Map.fromList fileRecords
            Left err -> do
              putMVar (cfFileRecords config) Map.empty
              throwM (err :: SomeException) -- purgeRecordsForChangedFilesOnFailure will handle this
          updateFileStates
    Left (_ :: SomeException) -> return () -- error is OK, postgresqlConnectionString may not be initialized at first

-- Listens to the compile process changing the DB when the source is recompiled
handleNotifications :: LspMonad ()
handleNotifications = do
  conn <- cfConnection <$> LSP.getConfig
  SQL.Notification _pid channel fileName <- liftIO $ readMVar conn >>= SQL.getNotification
  when (channel == "module_clean") $ do
    fileRecords <- liftLSP $ cfFileRecords <$> LSP.getConfig
    liftIO $ markFileRecordsClean [BS.unpack fileName] fileRecords
    updateFileStates
  handleNotifications

checkIfFilesHaveBeenChanged :: (ProgressAmount -> LspMonad ()) -> LspMonad [(FilePath, SourceDiffs Original Modified)]
checkIfFilesHaveBeenChanged report = do
  diffs <- getAllModifiedFileDiffs
  let numFiles = fromIntegral $ Prelude.length diffs
  forM ([0..] `zip` diffs) $ \(i, diff@(fp, _, _)) -> do
    res <- checkFileHaveBeenChanged diff
    report $
      ProgressAmount
        (Just $ round $ 100.0 * fromIntegral i / fromIntegral numFiles)
        (Just $ T.pack fp <> " (" <> T.pack (show (i :: Int)) <> "/" <> T.pack (show (numFiles :: Int)) <> ")")
    return res

purgeRecordsForChangedFilesOnFailure :: LspMonad () -> LspMonad ()
purgeRecordsForChangedFilesOnFailure action =
  action
    `catch` \(_ :: LSP.ProgressCancelledException) -> purgeAndNotify
    `catch` \e -> purgeAndNotify >> throwM (e :: SomeException)
  where
    purgeAndNotify = do
      sendError "Checking file status failed, some files will be removed from the DB"
      purgeRecordsForChangedFiles

purgeRecordsForChangedFiles :: LspMonad ()
purgeRecordsForChangedFiles = do
  diffs <- getAllModifiedFileDiffs
  forM_ diffs $ \diff -> do
    modificationTime <- getFileLaterModificationTime diff
    when (isJust modificationTime) $ cleanModuleFromDB $ diff ^. _1
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

