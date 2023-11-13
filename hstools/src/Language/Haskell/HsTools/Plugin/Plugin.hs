{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.HsTools.Plugin.Plugin where

import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Time.Clock
import Database.PostgreSQL.Simple (Connection, withTransaction, connectPostgreSQL)
import Options.Applicative
import Options.Applicative.Builder (defaultPrefs)
import Options.Applicative.Help.Chunk (stringChunk, (<</>>))
import System.Directory

import Plugins
import HscTypes
import HsDecls
import HsExtension
import TcRnTypes
import Module
import GHC
import IOEnv

import Language.Haskell.HsTools.Plugin.Monad
import Language.Haskell.HsTools.Plugin.StoreInfo
import Language.Haskell.HsTools.Plugin.Types
import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.Utils

plugin :: Plugin
plugin = defaultPlugin 
    { parsedResultAction = curry . wrapOptions parsedAction
    , renamedResultAction = curry . wrapOptions renamedAction
    , typeCheckResultAction = curry . wrapOptions typeCheckAction
    , pluginRecompile = const $ return NoForceRecompile
    , spliceRunAction = wrapOptions spliceAction
    }

wrapOptions :: MonadIO m => (PluginOptions -> c -> m a) -> [CommandLineOption] -> c -> m a
wrapOptions action opts input = do
  let parserInfo = info pluginOptions fullDesc
  let parserResult = execParserPure defaultPrefs parserInfo opts
  options <- liftIO $ handleParseResult $ addPluginWarning parserResult
  action options input
  where
    addPluginWarning = overFailure $ \p -> p { helpFooter = helpFooter p <</>> stringChunk warning }
    warning = "Please don't forget to add the \"Language.Haskell.HsTools.Plugin:\" prefix to all plugin options."

data PluginOptions = PluginOptions
  { poConnectionString :: String
  , poLogOptions :: LogOptions
  }

withDB :: MonadIO m => PluginOptions -> (Connection -> m a) -> m a
withDB pluginOpts action = do
  conn <- liftIO $ connectPostgreSQL $ BS.pack $ poConnectionString pluginOpts
  action conn

cleanAndRecordModule :: DbConn -> ModSummary -> IO ()
cleanAndRecordModule conn ms = do
  let Module unitId moduleName = ms_mod ms
  case ml_hs_file $ ms_location ms of
    Just filePath -> do
      fullPath <- canonicalizePath filePath
      maybeModificationTime <- getFileModificationTime fullPath
      currentTime <- getCurrentTime
      compiledTime <- runReaderT (getCompiledTime fullPath) conn
      needsUpdate <- case (compiledTime, maybeModificationTime) of
        (Just dbModDate, Just modificationTime) ->
          if dbModDate < modificationTime
            then do putStrLn $ "File " ++ fullPath ++ " is processed but not up to date, cleaning" ++ "  " ++ show dbModDate ++ "   " ++ show modificationTime 
                    runReaderT (cleanModuleFromDB fullPath) conn >> return True
            else do putStrLn $ "File " ++ fullPath ++ " is processed and up to date "
                    return False
        _ -> do
          putStrLn $ "File " ++ fullPath ++ " not processed yet."
          return True
      when needsUpdate $ do
        content <- readFileContent fullPath
        void $ runReaderT (insertModule fullPath (fromMaybe currentTime maybeModificationTime) (moduleNameString moduleName) (unitIdString unitId) (fromMaybe "" content)) conn
    Nothing -> putStrLn $ "Warning: haskell source not found for module " ++ show moduleName

parsedAction :: PluginOptions -> (ModSummary, HsParsedModule) -> Hsc HsParsedModule
parsedAction options (ms, mod) = liftIO $ do
  withDB options $ \conn -> do
    let dbConn = DbConn (poLogOptions options) conn
    runReaderT reinitializeTablesIfNeeded dbConn
    withTransaction conn $ cleanAndRecordModule dbConn ms
    doRunStage (poLogOptions options) conn "parse" ms (< SourceSaved) SourceSaved $ storeParsed mod
  return mod


renamedAction :: PluginOptions -> (TcGblEnv, HsGroup GhcRn) -> TcM (TcGblEnv, HsGroup GhcRn)
renamedAction options (env, group) = do
  let exports = maybe [] (map fst) $ tcg_rn_exports env
  let imports = tcg_rn_imports env
  -- Renamed action can be invoked multiple times to store the results after resolving template haskell
  runStage options "rename" (<= NamesLoaded) NamesLoaded $ storeNames (exports, imports, group)
  return (env, group)

typeCheckAction :: PluginOptions -> (ModSummary, TcGblEnv) -> TcM TcGblEnv
typeCheckAction options (ms, env) = withDB options $ \conn -> liftIO $ do
  let performStage = doRunStage (poLogOptions options) conn
  performStage "typeCheck" ms (< TypesLoaded) TypesLoaded $ storeTypes env
  performStage "main" ms (== TypesLoaded) TypesLoaded $ storeMain $ tcg_main env
  return env

spliceAction :: PluginOptions -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
spliceAction options expr = do
  runStage options "splice" (<= NamesLoaded) NamesLoaded $ storeTHNamesAndTypes expr
  return expr
  
runStage :: PluginOptions -> String -> (LoadingState -> Bool) -> LoadingState -> StoreStageM () -> TcM ()
runStage options caption condition newStage action = withDB options $ \conn -> do
  env <- getEnv
  let ms = mgLookupModule (hsc_mod_graph $ env_top env) (tcg_mod $ env_gbl env)
  liftIO $ case ms of
    Just ms' -> doRunStage (poLogOptions options) conn caption ms' condition newStage action
    Nothing -> putStrLn $ "Warning: ModSummary not found for " ++ show (tcg_mod $ env_gbl env)

doRunStage :: LogOptions -> Connection -> String -> ModSummary -> (LoadingState -> Bool) -> LoadingState -> StoreStageM () -> IO ()
doRunStage logOptions conn caption ms condition newStage action = do
  let dbConn = DbConn logOptions conn
      modName = moduleNameString $ moduleName $ ms_mod ms
  case ml_hs_file $ ms_location ms of
    Just filePath -> do
      filePath <- canonicalizePath filePath
      logStageTime caption modName logOptions $
        withTransaction conn $ do
          moduleIdAndState <- runReaderT (getModuleIdLoadingState filePath) dbConn
          case moduleIdAndState of
            Just (modId, status) | condition status -> do 
              runReaderT action $ dbConnToStoreParams (modName, modId) dbConn
              void $ liftIO $ runReaderT (updateLoadingState modId newStage) dbConn
            Nothing -> putStrLn $ "Warning: Module is not in the DB: " ++ modName ++
                        " this probably means that some problem happened in the earlier stages of the compilation"
            Just _ -> return () -- already done, module or dependencies did not change
    Nothing -> putStrLn $ "Warning: haskell source not found for module " ++ modName

logStageTime :: String -> String -> LogOptions -> IO () -> IO ()
logStageTime stage mod logOptions action = do
    when (logOptionsHighLevel logOptions) $
      putStrLn $ "Running stage " ++ stage ++ " on module " ++ mod
    startTime <- getCurrentTime
    res <- action
    endTime <- getCurrentTime
    when (logOptionsPerformance logOptions) $
      putStrLn $ "Stage " ++ stage ++ " took " ++ show (diffUTCTime endTime startTime) ++ " seconds"
    return res

pluginOptions :: Parser PluginOptions
pluginOptions =
  PluginOptions
    <$> connectionStringOption
    <*> logOption
  where
    connectionStringOption =
      argument str
        (metavar "DATABASE_CONNECTION_STRING" <>
          help "Database connection string (for example: postgresql://saver:saver@127.0.0.1:5432/repo)")
    logOption =
      LogOptions
        <$> switch (long "log-highlevel" <> help "Log high-level information, like when modules are processed")
        <*> switch (long "log-queries" <> help "Log individual queries toward the database")
        <*> switch (long "log-performance" <> help "Log performance-related information, like execution times")
        <*> switch (long "log-data" <> help "Log the full data of the operations with each database record")
