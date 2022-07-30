{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.HsTools.Plugin where

import Control.Monad.IO.Class
import Control.Monad
import System.Directory
import System.FilePath
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock

import Plugins
import HscTypes
import HsDecls
import HsExtension
import TcRnTypes
import Module
import FastString
import SrcLoc

import Language.Haskell.HsTools.PersistNameInfo

plugin :: Plugin
plugin = defaultPlugin 
    { parsedResultAction = parsedAction
    , renamedResultAction = renamedAction
    , typeCheckResultAction = typeCheckAction
    , pluginRecompile = const $ return NoForceRecompile
    }

withDB :: MonadIO m => [CommandLineOption] -> (Connection -> m a) -> m a
withDB [] _ = error "Cannot connect to DB, no connection string"
withDB (connectionString:_) action = do
    -- homeDir <- liftIO getHomeDirectory
    -- liftIO $ writeFile (homeDir </> "hstools.log") $ "Connecting to DB with '" ++ connectionString ++ "'"
    conn <- liftIO (connectPostgreSQL (BS.pack connectionString))
    action conn


-- needsSavingMS :: Connection -> ModSummary -> IO (Maybe Int)
-- needsSavingMS conn (ml_hs_file . ms_location -> Just hsFilePath) = 
--     checkCleanDBFilepath conn hsFilePath
-- needsSavingMS _ _ = return True
    
needsSavingGblEnv :: Connection -> TcGblEnv -> IO (Maybe (String, Int))
needsSavingGblEnv conn env = do
    currentDir <- getCurrentDirectory
    let filePath = normalise (currentDir </> localFilePath)
    fmap (fmap (modName ,)) $ checkCleanDBFilepath conn filePath
    where
        localFilePath = unpackFS $ srcSpanFile $ tcg_top_loc env
        modName = moduleNameString $ moduleName $ tcg_mod env

checkCleanDBFilepath :: Connection -> FilePath -> IO (Maybe Int)
checkCleanDBFilepath conn filePath = do
    modificationTime <- getModificationTime filePath
    let roundedModificationTime = roundUTCTime modificationTime
    res <- query conn "SELECT modifiedTime FROM modules WHERE filePath = ?" (Only filePath)
    needsUpdate <- case res of
        [] -> do liftIO $ putStrLn $ "File " ++ filePath ++ " not processed yet"
                 return True
        [dbModDate]:_ ->
            if dbModDate < roundedModificationTime
                then do liftIO $ putStrLn $ "File " ++ filePath ++ " is processed but not up to date, cleaning" ++ "  " ++ show dbModDate ++ "   " ++ show roundedModificationTime 
                        cleanModuleFromDB conn filePath >> return True
                else do liftIO $ putStrLn $ "File " ++ filePath ++ " is processed and up to date "
                        return False
        _ -> cleanModuleFromDB conn filePath >> return True
    if needsUpdate
        then Just . head . head <$> query conn "INSERT INTO modules (filePath, modifiedTime) VALUES (?, ?) RETURNING moduleId" (filePath, roundedModificationTime)
        else return Nothing

roundUTCTime :: UTCTime -> UTCTime
roundUTCTime (UTCTime day time) = UTCTime day (picosecondsToDiffTime $ round $ diffTimeToPicoseconds time)
  where round = (* 10^8) . (`div` 10^8)

cleanModuleFromDB :: Connection -> FilePath -> IO ()
cleanModuleFromDB conn filePath = 
    void $ execute conn "DELETE FROM modules WHERE filePath = ?" [filePath]

initializeTables :: Connection -> IO ()
initializeTables conn = mapM_ (execute_ conn) 
    [ "CREATE TABLE IF NOT EXISTS modules \
        \(moduleId INT GENERATED ALWAYS AS IDENTITY\
        \,PRIMARY KEY(moduleId)\
        \,filePath TEXT UNIQUE\
        \,modifiedTime TIMESTAMP WITH TIME ZONE\
        \);"
    , "CREATE TABLE IF NOT EXISTS names \
        \(module INT\
        \,CONSTRAINT fk_module FOREIGN KEY(module) REFERENCES modules(moduleId) ON DELETE CASCADE\
        \,isDefined BOOL\
        \,name TEXT\
        \,startRow INT\
        \,startColumn INT\
        \,endRow INT\
        \,endColumn INT\
        \);"
    , "CREATE INDEX IF NOT EXISTS index_names_range ON names (startRow, endRow, startColumn, endColumn);"
    ]

parsedAction :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedAction _ _ mod = do
    return mod

renamedAction :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
renamedAction clOpts env group = withDB clOpts $ \conn -> do
    liftIO $ withTransaction conn $ do
        initializeTables conn
        moduleIdIfNeedToSave <- needsSavingGblEnv conn env
        case moduleIdIfNeedToSave of
            Just moduleNameAndId -> storeNames conn moduleNameAndId group
            Nothing -> return ()
    --liftIO $ putStrLn $ showSDocUnsafe $ ppr group
    return (env, group)

typeCheckAction :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typeCheckAction _ _ env = do
    --liftIO $ putStrLn $ showSDocUnsafe $ ppr (tcg_binds env)
    return env