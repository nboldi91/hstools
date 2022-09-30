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

module Language.Haskell.HsTools.LspServer where

import Language.LSP.Server as LSP
import Language.LSP.Types as LSP
import Language.LSP.Types.Lens as LSP (uri, params, settings, changes)
import Control.Monad.IO.Class
import Control.Monad
import Control.Exception
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isNothing, catMaybes)
import Data.String (IsString(..))
import Control.Lens hiding (Iso)
import Data.Time.Clock
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import qualified Database.PostgreSQL.Simple.Notification as SQL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Vector as V
import qualified Data.Aeson.KeyMap as KM
import Text.Read (readMaybe)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import System.IO
import Colog.Core.Action

import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.LinesDiff

handlers :: Handlers (LspM Config)
handlers = mconcat
  [ notificationHandler SInitialized $ \_ -> runInContext "Initialization" tryToConnectToDB
  , notificationHandler SWorkspaceDidChangeConfiguration $ \_ -> runInContext "ChangeConfiguration" $ do
      cfg <- liftLSP LSP.getConfig
      when (isNothing (cfConnection cfg))
        tryToConnectToDB
      
  , requestHandler STextDocumentDefinition $ \req responder -> runInContext "Definition" $ do
      let RequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier uri) pos _ _) = req
      withConnection $ \conn ->
        ensureFileLocation uri responder $ \file -> do
          rewrites <- getRewrites file
          case newToOriginalPos rewrites (posToSP pos) of
            Right originalPos -> do
              names <- liftIO $ getMatchingNames conn file (spLine originalPos) (spCol originalPos) (Just True)
              liftLSP $ responder $ Right $ InR $ InL $ LSP.List $ take 1 $ catMaybes $ map (lineToLoc rewrites) names
            Left _ -> liftLSP $ responder $ Right $ InR $ InL $ LSP.List [] -- the source position was not in the compiled source code

  , requestHandler STextDocumentReferences $ \req responder -> runInContext "References" $ do
      let RequestMessage _ _ _ (ReferenceParams (TextDocumentIdentifier uri) pos _ _ (ReferenceContext includeDefinition)) = req
      withConnection $ \conn ->
        ensureFileLocation uri responder $ \file -> do
          rewrites <- getRewrites file
          case newToOriginalPos rewrites (posToSP pos) of
            Right originalPos -> do
              names <- liftIO $ getMatchingNames conn file (spLine originalPos) (spCol originalPos) (if not includeDefinition then Just False else Nothing)
              liftLSP $ responder $ Right $ LSP.List $ catMaybes $ map (lineToLoc rewrites) names
            Left _ -> liftLSP $ responder $ Right $ LSP.List [] -- the source position was not in the compiled source code

  , requestHandler STextDocumentHover $ \req responder -> runInContext "Hover" $ do
      let RequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) pos _workDone) = req
      withConnection $ \conn ->
        ensureFileLocation uri responder $ \file -> do
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

  , notificationHandler (SCustomMethod "CleanDB") $ \message -> runInContext "CleanDB" $ withConnection $ \conn -> do
      let NotificationMessage _ _ args = message
      case args of
        A.Array (V.toList -> [ A.Null ]) -> do
          liftIO $ reinitializeTables conn
          sendMessage "DB cleaned"
        A.Array (V.toList -> [ A.String s ])
          -> sendMessage $ "Cleaning DB for path: " <> s
        _ -> sendError $ T.pack $ "Unrecognized CleanDB argument: " ++ show args
      updateFileStates

  , notificationHandler SWorkspaceDidChangeWatchedFiles $ \message -> runInContext "FileChange" $ withConnection $ \conn -> do
      let NotificationMessage _ _ (DidChangeWatchedFilesParams (LSP.List fileChanges)) = message
      forM_ fileChanges $ \(FileEvent uri _) -> ensureFileLocation' uri $ \filePath -> do
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
  
  , notificationHandler STextDocumentDidChange $ \msg -> runInContext "STextDocumentDidChange" $ withConnection $ \conn -> do
      let NotificationMessage _ _ (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _version) (LSP.List changes)) = msg
      ensureFileLocation' uri $ \filePath -> do
        let goodChanges = catMaybes $ map textDocChangeToSD changes
        liftLSP LSP.getConfig >>= \cf -> liftIO $ updateSavedFileRecords filePath goodChanges (cfFileRecords cf)
        updateFileStatesFor filePath

  , notificationHandler STextDocumentDidSave $ \msg -> runInContext "STextDocumentDidSave" $ withConnection $ \conn -> do
      let NotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) _reason) = msg
      ensureFileLocation' uri $ \filePath -> do
        cfg <- LSP.getConfig
        fileDiffs <- liftIO $ readMVar (cfFileRecords cfg) >>= return . maybe Map.empty frDiffs . Map.lookup filePath
        currentTime <- liftIO getCurrentTime
        let serializedDiff = serializeSourceDiffs fileDiffs
        void $ liftIO $ updateFileDiffs conn filePath currentTime (Just serializedDiff)
  
  , notificationHandler STextDocumentDidOpen $ \msg -> runInContext "STextDocumentDidOpen" $ withConnection $ \conn -> do
      let NotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _langId _version content)) = msg
      ensureFileLocation' uri $ \filePath ->
        liftLSP LSP.getConfig >>= \cf -> liftIO $ recordFileOpened conn filePath content (cfFileRecords cf)
  
  , notificationHandler STextDocumentDidClose $ \msg -> runInContext "STextDocumentDidClose" $ withConnection $ \conn -> do
      let NotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri)) = msg
      ensureFileLocation' uri $ \filePath ->
        liftLSP LSP.getConfig >>= \cf -> liftIO $ recordFileClosed conn filePath (cfFileRecords cf)
 
  ]

data LspContext = LspContext { ctOperation :: String }

type LspMonad = ReaderT LspContext (LspM Config)

liftLSP :: LspM Config a -> LspMonad a
liftLSP = lift

runInContext :: String -> LspMonad a -> LspM Config a
runInContext operation action = runReaderT action (LspContext { ctOperation = operation })

withConnection :: (Connection -> LspMonad ()) -> LspMonad ()
withConnection act = do
  operation <- asks ctOperation
  cfg <- lift LSP.getConfig
  case cfConnection cfg of
    Just conn -> act conn
    Nothing -> sendError $ T.pack $ operation ++ " needs DB connection"

ensureFileLocation :: Uri -> (Either ResponseError a -> LspM Config ()) -> (FilePath -> LspMonad ()) -> LspMonad ()
ensureFileLocation location responder action = case uriToFilePath location of
  Just fp -> action fp
  Nothing -> do
    operation <- asks ctOperation
    liftLSP $ responder $ Left $ responseError $ T.pack $ "Can't " ++ operation ++ ": Document is not a file"

ensureFileLocation' :: Uri -> (FilePath -> LspMonad ()) -> LspMonad ()
ensureFileLocation' location action = case uriToFilePath location of
  Just fp -> action fp
  Nothing -> do
    operation <- asks ctOperation
    sendError $ T.pack $ "Can't " ++ operation ++ ": Document is not a file"

responseError m = ResponseError InvalidRequest m Nothing

sendMessage :: T.Text -> LspMonad ()
sendMessage = liftLSP . sendNotification SWindowShowMessage . ShowMessageParams MtInfo

sendError :: T.Text -> LspMonad ()
sendError = liftLSP . sendNotification SWindowShowMessage . ShowMessageParams MtError

logMessage :: T.Text -> LspMonad ()
logMessage = liftLSP . sendNotification SWindowLogMessage . LogMessageParams MtInfo

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

updateFileStatesIO :: FilePath -> FileRecords -> (FromServerMessage -> IO ()) -> IO ()
updateFileStatesIO filePath fileRecords messageHandler = do
  fr <- readMVar fileRecords
  let status = Map.lookup filePath $ Map.map frDiffs fr
  sendFileStatesIO (maybe [] ((:[]) . (filePath,)) status) messageHandler

sendFileStatesIO :: [(FilePath, SourceDiffs)] -> (FromServerMessage -> IO ()) -> IO ()
sendFileStatesIO [] _ = return () 
sendFileStatesIO states messageHandler
  = messageHandler $ FromServerMess changeFileStatesMethod 
      $ NotMess $ NotificationMessage "2.0" changeFileStatesMethod $ createChangeFileStates states

createChangeFileStates :: [(FilePath, SourceDiffs)] -> A.Value
createChangeFileStates states = A.Object $ KM.fromList [ ("result", payload )]
  where
    payload = A.Array $ V.fromList $ map diffToStates states
    diffToStates (fp, diff) = A.Object $ KM.fromList [("filePath", A.String $ T.pack fp), ("state", toState diff)]
    toState diff = A.String $ if Map.null diff then "fresh" else "edited"

changeFileStatesMethod = SCustomMethod "ChangeFileStates"

-- Listens to the compile process changing the DB when the source is recompiled
handleNotifications :: Connection -> FileRecords -> (FromServerMessage -> IO ()) -> IO ()
handleNotifications conn fileRecords messageHandler = do
  SQL.Notification _pid channel fileName <- SQL.getNotification conn
  when (channel == "module_clean") $ do
    markFileRecordsClean [BS.unpack fileName] fileRecords
    updateFileStatesIO (BS.unpack fileName) fileRecords messageHandler
  handleNotifications conn fileRecords messageHandler

getRewrites :: FilePath -> LspMonad SourceDiffs
getRewrites fp = do
  cfg <- liftLSP LSP.getConfig
  fileRecords <- liftIO $ readMVar $ cfFileRecords cfg
  return $ maybe Map.empty frDiffs $ Map.lookup fp fileRecords

tryToConnectToDB :: LspMonad ()
tryToConnectToDB = do
  config <- liftLSP LSP.getConfig
  connOrError <- liftIO $ try $ connectPostgreSQL (BS.pack (cfPostgresqlConnectionString config))
  case connOrError of
    Right conn -> do 
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
    Left (e :: SomeException) -> return () -- error is OK

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

loadConfig :: Config -> A.Value -> Either T.Text Config
loadConfig config (A.Object (A.lookup "hstools" -> (Just (A.Object assoc))))
  = Right $ config 
  { cfPostgresqlConnectionString = fromMaybe (cfPostgresqlConnectionString config) $
      T.unpack <$> (fromString =<< psqlstr)
  }
  where
    psqlstr = A.lookup "postgresqlConnectionString" assoc
    fromString (A.String t) = Just t
    fromString _ = Nothing
loadConfig _ v = Left $ T.pack $ "Cannot parse options: " ++ show v

data FileRecord
  = FileRecord { frDiffs :: SourceDiffs }
  | OpenFileRecord 
    { frDiffs :: SourceDiffs
    , frCompiledContent :: FileLines
    , frCurrentContent :: FileLines
    }
  deriving (Show)

-- Nothing means it is not loaded yet from database
type FileRecords = MVar (Map.Map FilePath FileRecord)

recordFileOpened :: Connection -> FilePath -> T.Text -> FileRecords -> IO ()
recordFileOpened conn fp content mv
  = modifyMVar_ mv $ \frs -> updateRecord (Map.lookup fp frs) >>= \r -> return $ Map.alter (const r) fp frs
  where
    updateRecord Nothing =
      fmap (\(src, diffs) -> OpenFileRecord (deserializeSourceDiffs diffs) (toFileLines src) contentLines) 
        <$> getCompiledSourceAndModifiedFileDiffs conn fp
    updateRecord (Just (FileRecord diffs)) = do
      compiledSource <- getCompiledSource conn fp
      return $ fmap (\src -> OpenFileRecord diffs (toFileLines src) contentLines) compiledSource
    contentLines = toFileLines $ T.unpack content

recordFileClosed :: Connection -> FilePath -> FileRecords -> IO ()
recordFileClosed conn fp mv
  = modifyMVar_ mv $ \frs -> updateRecord (Map.lookup fp frs) >>= \r -> return $ Map.insert fp r frs
  where
    updateRecord (Just (OpenFileRecord diffs compiledContent currentContent)) = do
      modifiedDiffs <- getModifiedFileDiffs conn fp
      return $ FileRecord $ maybe Map.empty (Map.fromAscList . map read . lines) modifiedDiffs
    updateRecord _ = error $ "recordFileClosed: file " ++ fp ++ " should have been on record"

markFileRecordsClean :: [FilePath] -> FileRecords -> IO ()
markFileRecordsClean files
  = modifyMVarPure $ Map.mapWithKey (\fp fr -> if fp `elem` files then fr{frDiffs = Map.empty} else fr)

updateSavedFileRecords :: FilePath -> [SourceRewrite] -> FileRecords -> IO ()
updateSavedFileRecords fp newDiffs = modifyMVarPure $ Map.adjust updateDiffs fp
  where
    updateDiffs (OpenFileRecord diffs compiledContent currentContent) =
      let (currentContent', diffs') = foldr (addExtraChange compiledContent) (currentContent, diffs) newDiffs
      in OpenFileRecord diffs' compiledContent currentContent'
    updateDiffs fr = fr

replaceSourceDiffs :: FilePath -> SourceDiffs -> FileRecords -> IO ()
replaceSourceDiffs fp diffs = modifyMVarPure $ Map.adjust (\fr -> fr{frDiffs = diffs}) fp

isFileOpen :: FilePath -> FileRecords -> IO Bool
isFileOpen fp frsMVar = do
  frs <- readMVar frsMVar
  return $ case Map.lookup fp frs of
    Just OpenFileRecord{} -> True
    _ -> False

modifyMVarPure :: (a -> a) -> MVar a -> IO ()
modifyMVarPure f mv = modifyMVar_ mv (return . f)

data Config = Config
  { cfPostgresqlConnectionString :: String
  , cfConnection :: Maybe Connection
  , cfOperation :: Maybe String
  , cfFileRecords :: FileRecords
  }

hsToolsDefaultConfig :: FileRecords -> Config
hsToolsDefaultConfig fr = Config
  { cfPostgresqlConnectionString = ""
  , cfConnection = Nothing
  , cfOperation = Nothing
  , cfFileRecords = fr
  }

instance Show Connection where
  show _ = "<Connection>"

textDocChangeToSD :: LSP.TextDocumentContentChangeEvent -> Maybe SourceRewrite
textDocChangeToSD (LSP.TextDocumentContentChangeEvent (Just (LSP.Range start end)) _ text)
  = Just $ SourceRewrite st (posToSP end) (T.unpack text)
  where st = posToSP start
textDocChangeToSD _ = Nothing

posToSP :: LSP.Position -> SP
posToSP (LSP.Position line char) = SP (fromIntegral line + 1) (fromIntegral char + 1)

spToPos :: SP -> LSP.Position
spToPos (SP line char) = LSP.Position (fromIntegral line - 1) (fromIntegral char - 1)

rangeToLSP :: SourceRange -> LSP.Range
rangeToLSP (SourceRange start end) = LSP.Range (spToPos start) (spToPos end)

lineToLoc :: SourceDiffs -> (String, Int, Int, Int, Int) -> Maybe LSP.Location
lineToLoc rewrites (file, startLine, startCol, endLine, endCol)
  = fmap (LSP.Location (filePathToUri file) . rangeToLSP) 
      $ originalToNewRangeStrict rewrites 
      $ SourceRange (SP startLine startCol) (SP endLine endCol)

nothingIfEmpty :: String -> Maybe String
nothingIfEmpty "" = Nothing
nothingIfEmpty str = Just str

serializeSourceDiffs :: SourceDiffs -> String
serializeSourceDiffs = unlines . map show . Map.toAscList 

deserializeSourceDiffs :: String -> SourceDiffs
deserializeSourceDiffs str
  = Map.fromAscList $ map (\s -> fromMaybe (error $ "Can't deserialize source diff: " ++ show s) $ readMaybe s) $ lines str
