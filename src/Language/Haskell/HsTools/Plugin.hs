{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.HsTools.Plugin where

import Control.Monad.IO.Class
import Control.Monad
import System.Directory
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock
import Data.Char (toLower)

import qualified FastString as FS
import Plugins
import HscTypes
import HsDecls
import HsExtension
import TcRnTypes
import Module
import HsExpr
import IOEnv
import SrcLoc

import Language.Haskell.HsTools.PersistNameInfo

plugin :: Plugin
plugin = defaultPlugin 
    { parsedResultAction = parsedAction
    , renamedResultAction = renamedAction
    , typeCheckResultAction = typeCheckAction
    , pluginRecompile = const $ return NoForceRecompile
    , spliceRunAction = spliceAction
    }

withDB :: MonadIO m => [CommandLineOption] -> (Connection -> m a) -> m a
withDB [] _ = error "Cannot connect to DB, no connection string"
withDB (connectionString:_) action = do
    conn <- liftIO (connectPostgreSQL (BS.pack connectionString))
    action conn

cleanAndRecordModule :: Connection -> ModSummary -> IO (Maybe Int)
cleanAndRecordModule conn ms = do
    let Module unitId moduleName = ms_mod ms
    case ml_hs_file $ ms_location ms of
        Just filePath -> do
            fullPath <- canonicalizePath filePath
            modificationTime <- getModificationTime fullPath
            let roundedModificationTime = roundUTCTime modificationTime
            res <- query conn "SELECT compiledTime FROM modules WHERE filePath = ?" (Only fullPath)
            needsUpdate <- case res of
                [] -> do
                    putStrLn $ "File " ++ fullPath ++ " not processed yet."
                    return True
                [dbModDate]:_ ->
                    if dbModDate < roundedModificationTime
                        then do putStrLn $ "File " ++ fullPath ++ " is processed but not up to date, cleaning" ++ "  " ++ show dbModDate ++ "   " ++ show roundedModificationTime 
                                cleanModuleFromDB conn fullPath >> return True
                        else do putStrLn $ "File " ++ fullPath ++ " is processed and up to date "
                                return False
                _ -> cleanModuleFromDB conn fullPath >> return True
            if needsUpdate
                then do
                  content <- readFile fullPath
                  [[res]] <- query conn "INSERT INTO modules (filePath, compiledTime, moduleName, unitId, loadingState, compiledSource, modifiedTime, modifiedFileDiffs) VALUES (?, ?, ?, ?, 0, ?, null, null) RETURNING moduleId"
                                (fullPath, roundedModificationTime, moduleNameString moduleName, unitIdString unitId, content)
                  return $ Just res
                else return Nothing
        Nothing -> return Nothing

getModuleIdAndState :: Connection -> FilePath -> IO (Maybe (Int, LoadingState))
getModuleIdAndState conn fp = do
    res <- query conn "SELECT moduleId, loadingState FROM modules WHERE filePath = ?" (Only fp)
    case res of
        [(moduleId, loadingState)] -> return $ Just (moduleId, toEnum loadingState)
        _ -> return Nothing

updateLoadingState :: Connection -> Int -> LoadingState -> IO ()
updateLoadingState conn moduleId newLoadingState =
    void $ execute conn
        "UPDATE modules SET loadingState = ? WHERE moduleId = ?"
        (fromEnum newLoadingState, moduleId)


roundUTCTime :: UTCTime -> UTCTime
roundUTCTime (UTCTime day time) = UTCTime day (picosecondsToDiffTime $ round $ diffTimeToPicoseconds time)
  where round = (* 10^8) . (`div` 10^8)

cleanModuleFromDB :: Connection -> FilePath -> IO ()
cleanModuleFromDB conn filePath = do
    moduleIds <- query conn "SELECT moduleId FROM modules WHERE filePath = ?" [filePath]
    forM_ (moduleIds :: [[Int]]) $ \[moduleId] -> do
      void $ execute conn "DELETE FROM names WHERE module = ?" [moduleId]
      void $ execute conn "DELETE FROM types WHERE module = ?" [moduleId]
      void $ execute conn "DELETE FROM thRanges WHERE module = ?" [moduleId]
      void $ execute conn "DELETE FROM ast WHERE module = ?" [moduleId]
      void $ execute conn "DELETE FROM modules WHERE moduleId = ?" [moduleId]

initializeTables :: Connection -> IO ()
initializeTables conn = do 
    tables <- query_ conn "SELECT tablename FROM pg_tables"
    indices <- query_ conn "SELECT indexname FROM pg_indexes"
    let allExistingDefs = map (toLowerCase . head) $ tables ++ indices
    mapM_ (execute_ conn) (map snd . filter ((`notElem` allExistingDefs) . toLowerCase . fst) $ tableDefs)
  where
    toLowerCase = map toLower

    tableDefs :: [(String, Query)]
    tableDefs =
      [ ("modules", "CREATE TABLE modules \
          \(moduleId SERIAL PRIMARY KEY\
          \,filePath TEXT UNIQUE NOT NULL\
          \,unitId TEXT NOT NULL\
          \,moduleName TEXT NOT NULL\
          \,compiledTime TIMESTAMP WITH TIME ZONE NOT NULL\
          \,loadingState INT NOT NULL\
          \,compiledSource TEXT NOT NULL\
          \,modifiedTime TIMESTAMP WITH TIME ZONE\
          \,modifiedFileDiffs TEXT\
          \);"
      )
      , ("ast", "CREATE TABLE ast \
          \(module INT NOT NULL\
          \,CONSTRAINT fk_ast_module FOREIGN KEY(module) REFERENCES modules(moduleId)\
          \,astId SERIAL PRIMARY KEY\
          \,startRow INT NOT NULL\
          \,startColumn INT NOT NULL\
          \,endRow INT NOT NULL\
          \,endColumn INT NOT NULL\
          \);"
      )
      , ("names", "CREATE TABLE names \
          \(module INT NOT NULL\
          \,astNode INT NOT NULL\
          \,CONSTRAINT fk_name_ast FOREIGN KEY(astNode) REFERENCES ast(astId)\
          \,isDefined BOOL NOT NULL\
          \,name TEXT NOT NULL\
          \,namespace INT\
          \);"
      )
      , ("types", "CREATE TABLE types \
          \(module INT NOT NULL\
          \,astNode INT NOT NULL\
          \,CONSTRAINT fk_type_ast FOREIGN KEY(astNode) REFERENCES ast(astId)\
          \,type TEXT NOT NULL\
          \);"
      )
      , ("thRanges", "CREATE TABLE thRanges \
          \(module INT NOT NULL\
          \,astNode INT NOT NULL\
          \,CONSTRAINT fk_type_ast FOREIGN KEY(astNode) REFERENCES ast(astId)\
          \);"
      )
      , ("notifyModulesFunction", "CREATE OR REPLACE FUNCTION notifyModulesFunction() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.modifiedFileDiffs IS NULL THEN PERFORM pg_notify('module_clean', NEW.filePath); END IF; RETURN NEW; END; $$")
      , ("notifyModulesTrigger", "CREATE OR REPLACE TRIGGER notifyModulesTrigger AFTER INSERT OR UPDATE ON modules FOR EACH ROW EXECUTE FUNCTION notifyModulesFunction()")
      ]

data LoadingState = NotLoaded | SourceSaved | NamesLoaded | TypesLoaded
    deriving (Show, Enum, Eq, Ord)

parsedAction :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedAction clOpts ms mod = liftIO $ do
    withDB clOpts $ \conn -> do
        initializeTables conn
        cleanAndRecordModule conn ms
    return mod

renamedAction :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
renamedAction clOpts env group = do
  runStage clOpts "rename" (<= NamesLoaded) NamesLoaded (flip storeNames group)
  return (env, group)

typeCheckAction :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typeCheckAction clOpts _ env = do
  runStage clOpts "typeCheck" (< TypesLoaded) TypesLoaded (flip storeTypes env)
  return env

spliceAction :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
spliceAction clOpts expr = do
  runStage clOpts "splice" (<= NamesLoaded) NamesLoaded (flip storeTHNamesAndTypes expr)
  return expr
  
runStage :: [CommandLineOption] -> String -> (LoadingState -> Bool) -> LoadingState -> (StoreParams -> IO ()) -> TcM ()
runStage clOpts caption condition newStage action = withDB clOpts $ \conn -> do
  env <- getEnv
  let mod = tcg_mod $ env_gbl env
      modName = moduleNameString $ moduleName mod
      localFilePath = FS.unpackFS $ srcSpanFile $ tcg_top_loc $ env_gbl env
  fullFilePath <- liftIO $ canonicalizePath localFilePath
  liftIO $ withTransaction conn $ do
    moduleIdAndState <- getModuleIdAndState conn fullFilePath
    case moduleIdAndState of
      Just (modId, status) | condition status -> do 
        action $ StoreParams (isVerbose clOpts) conn (modName, modId)
        void $ updateLoadingState conn modId newStage
      Nothing -> putStrLn $ "[" ++ caption ++ "] WARNING: Module is not in the DB: " ++ modName
      Just _ -> return ()

isVerbose :: [CommandLineOption] -> Bool
isVerbose (_:"verbose":_) = True
isVerbose _ = False
