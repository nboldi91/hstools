{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.HsTools.Plugin.Plugin where

import Control.Monad.IO.Class
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Time.Clock
import Database.PostgreSQL.Simple (Connection, withTransaction, connectPostgreSQL)
import System.Directory

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

import Language.Haskell.HsTools.Plugin.StoreInfo
import Language.Haskell.HsTools.Plugin.Types
import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.HandleErrors
import Language.Haskell.HsTools.Utils

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

cleanAndRecordModule :: Connection -> ModSummary -> IO (Maybe FilePath)
cleanAndRecordModule conn ms = do
  let Module unitId moduleName = ms_mod ms
  case ml_hs_file $ ms_location ms of
    Just filePath -> do
      fullPath <- canonicalizePath filePath
      maybeModificationTime <- getFileModificationTime fullPath
      currentTime <- getCurrentTime
      compiledTime <- getCompiledTime conn fullPath
      needsUpdate <- case (compiledTime, maybeModificationTime) of
        (Just dbModDate, Just modificationTime) ->
          if dbModDate < modificationTime
            then do putStrLn $ "File " ++ fullPath ++ " is processed but not up to date, cleaning" ++ "  " ++ show dbModDate ++ "   " ++ show modificationTime 
                    cleanModuleFromDB conn fullPath >> return True
            else do putStrLn $ "File " ++ fullPath ++ " is processed and up to date "
                    return False
        _ -> do
          putStrLn $ "File " ++ fullPath ++ " not processed yet."
          return True
      if needsUpdate
          then do
            content <- readFileContent fullPath
            Just <$> insertModule conn fullPath (fromMaybe currentTime maybeModificationTime) (moduleNameString moduleName) (unitIdString unitId) (fromMaybe "" content)
          else return Nothing
      return $ Just fullPath
    Nothing -> return Nothing

parsedAction :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedAction clOpts ms mod = liftIO $ do
  when (isVerbose clOpts) $ putStrLn $ "Starting stage: parsedAction"
  withDB clOpts $ \conn -> handleErrors conn "parsedAction" $ do
    reinitializeTablesIfNeeded conn
    moduleFilePath <- cleanAndRecordModule conn ms
    let modName = moduleNameString $ ms_mod_name ms
    case moduleFilePath of
      Just fp -> doRunStage (isVerbose clOpts) conn "parse" fp modName (< SourceSaved) SourceSaved (flip storeParsed mod)
      Nothing -> return ()
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
  when (isVerbose clOpts) $ liftIO $ putStrLn $ "Starting stage: " ++ caption
  env <- getEnv
  let mod = tcg_mod $ env_gbl env
      modName = moduleNameString $ moduleName mod
      localFilePath = FS.unpackFS $ srcSpanFile $ tcg_top_loc $ env_gbl env
  fullFilePath <- liftIO $ canonicalizePath localFilePath
  liftIO $ doRunStage (isVerbose clOpts) conn caption fullFilePath modName condition newStage action

doRunStage :: Bool -> Connection -> String -> FilePath -> String -> (LoadingState -> Bool) -> LoadingState -> (StoreParams -> IO ()) -> IO ()
doRunStage isVerbose conn caption fullFilePath modName condition newStage action =
  handleErrors conn ("plugin: " ++ caption) $ withTransaction conn $ do
    moduleIdAndState <- getModuleIdLoadingState conn fullFilePath
    case moduleIdAndState of
      Just (modId, status) | condition status -> do 
        action $ StoreParams isVerbose conn (modName, modId)
        void $ updateLoadingState conn modId newStage
      Nothing -> putStrLn $ "[" ++ caption ++ "] WARNING: Module is not in the DB: " ++ modName
      Just _ -> return ()

isVerbose :: [CommandLineOption] -> Bool
isVerbose (_:"verbose":_) = True
isVerbose _ = False
