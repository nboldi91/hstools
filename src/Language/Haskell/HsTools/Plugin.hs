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

import Plugins
import HscTypes
import HsDecls
import HsExtension
import TcRnTypes
import Module
import HsExpr
import IOEnv

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
            res <- query conn "SELECT modifiedTime FROM modules WHERE filePath = ?" (Only fullPath)
            needsUpdate <- case res of
                [] -> do
                    liftIO $ putStrLn $ "File " ++ fullPath ++ " not processed yet"
                    return True
                [dbModDate]:_ ->
                    if dbModDate < roundedModificationTime
                        then do liftIO $ putStrLn $ "File " ++ fullPath ++ " is processed but not up to date, cleaning" ++ "  " ++ show dbModDate ++ "   " ++ show roundedModificationTime 
                                cleanModuleFromDB conn fullPath >> return True
                        else do liftIO $ putStrLn $ "File " ++ fullPath ++ " is processed and up to date "
                                return False
                _ -> cleanModuleFromDB conn fullPath >> return True
            if needsUpdate
                then Just . head . head <$>
                    query conn "INSERT INTO modules (filePath, modifiedTime, moduleName, unitId, loadingState) VALUES (?, ?, ?, ?, ?) RETURNING moduleId"
                        (fullPath, roundedModificationTime, moduleNameString moduleName, unitIdString unitId, 0 :: Int)
                else return Nothing
        Nothing -> return Nothing

getModuleIdAndState :: Connection -> Module -> IO (Maybe (Int, LoadingState))
getModuleIdAndState conn (Module unitId moduleName) = do
    res <- query conn
        "SELECT moduleId, loadingState FROM modules WHERE moduleName = ? AND unitId = ?"
        (moduleNameString moduleName, unitIdString unitId)
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
cleanModuleFromDB conn filePath = 
    void $ execute conn "DELETE FROM modules WHERE filePath = ?" [filePath]

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
          \,modifiedTime TIMESTAMP WITH TIME ZONE NOT NULL\
          \,loadingState INT NOT NULL\
          \);"
      )
      , ("ast", "CREATE TABLE ast \
          \(module INT NOT NULL\
          \,CONSTRAINT fk_ast_module FOREIGN KEY(module) REFERENCES modules(moduleId) ON DELETE CASCADE\
          \,astId SERIAL PRIMARY KEY\
          \,startRow INT NOT NULL\
          \,startColumn INT NOT NULL\
          \,endRow INT NOT NULL\
          \,endColumn INT NOT NULL\
          \);"
      )
      , ("names", "CREATE TABLE names \
          \(astNode INT NOT NULL\
          \,CONSTRAINT fk_name_ast FOREIGN KEY(astNode) REFERENCES ast(astId) ON DELETE CASCADE\
          \,isDefined BOOL NOT NULL\
          \,name TEXT NOT NULL\
          \,namespace INT\
          \);"
      )
      , ("types", "CREATE TABLE types \
          \(astNode INT NOT NULL\
          \,CONSTRAINT fk_type_ast FOREIGN KEY(astNode) REFERENCES ast(astId) ON DELETE CASCADE\
          \,type TEXT NOT NULL\
          \);"
      )
      , ("thRanges", "CREATE TABLE thRanges \
          \(astNode INT NOT NULL\
          \,CONSTRAINT fk_type_ast FOREIGN KEY(astNode) REFERENCES ast(astId) ON DELETE CASCADE\
          \);"
      )
      , ("index_ast_range", "CREATE INDEX index_ast_range ON ast (startRow, endRow, startColumn, endColumn);")
      ]

data LoadingState = NotLoaded | NamesLoaded | TypesLoaded
    deriving (Show, Enum, Eq, Ord)

parsedAction :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedAction clOpts ms mod = liftIO $ do
    withDB clOpts $ \conn -> do
        initializeTables conn
        cleanAndRecordModule conn ms
    return mod

renamedAction :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
renamedAction clOpts env group = withDB clOpts $ \conn -> do
  liftIO $ withTransaction conn $ do
    let mod = tcg_mod env
        modName = moduleNameString $ moduleName mod
    moduleIdAndState <- getModuleIdAndState conn mod
    case moduleIdAndState of
      Just (modId, status) | status <= NamesLoaded {- renaming can happen multiple times -} -> do 
        storeNames (isVerbose clOpts) conn (modName, modId) group
        void $ updateLoadingState conn modId NamesLoaded
      Nothing -> liftIO $ putStrLn $ "Module is not in the DB: " ++ modName
      Just _ -> liftIO $ putStrLn $ "Skipping module: " ++ modName
  return (env, group)

typeCheckAction :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typeCheckAction clOpts _ env = withDB clOpts $ \conn -> liftIO $ withTransaction conn $ do
  let mod = tcg_mod env
      modName = moduleNameString $ moduleName mod
  moduleIdAndState <- getModuleIdAndState conn mod
  case moduleIdAndState of
    Just (modId, status) | status < TypesLoaded -> do 
      storeTypes (isVerbose clOpts) conn (modName, modId) env
      void $ updateLoadingState conn modId NamesLoaded
    Nothing -> liftIO $ putStrLn $ "Module is not in the DB: " ++ modName
    Just _ -> liftIO $ putStrLn $ "Skipping module: " ++ modName
  return env

spliceAction :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
spliceAction clOpts expr = withDB clOpts $ \conn -> do
  env <- getEnv
  let mod = tcg_mod $ env_gbl env
      modName = moduleNameString $ moduleName mod
  liftIO $ withTransaction conn $ do
    moduleIdAndState <- getModuleIdAndState conn mod
    case moduleIdAndState of
      Just (modId, status) | status <= NamesLoaded -> do 
        storeTHNamesAndTypes (isVerbose clOpts) conn (modName, modId) expr
      Nothing -> liftIO $ putStrLn $ "Module is not in the DB: " ++ modName
      Just _ -> liftIO $ putStrLn $ "Skipping module: " ++ modName
    return expr

isVerbose :: [CommandLineOption] -> Bool
isVerbose (_:"verbose":_) = True
isVerbose _ = False
