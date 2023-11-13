{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Haskell.HsTools.LspServer.Monad where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.IO.Unlift
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time.Clock
import Database.PostgreSQL.Simple (Connection)
import Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP

import Language.Haskell.HsTools.LspServer.State
import Language.Haskell.HsTools.LspServer.FileRecords
import Language.Haskell.HsTools.LspServer.Utils
import Language.Haskell.HsTools.SourceDiffs
import Language.Haskell.HsTools.SourcePosition
import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.Utils (LogOptions(..), DbConn(..))

data LspContext = LspContext { ctOperation :: String }

type Lsp = LspM Config

type LspMonad = ReaderT LspContext Lsp

liftLSP :: LspM Config a -> LspMonad a
liftLSP = lift

runInContext :: String -> LspMonad a -> Lsp a
runInContext operation action = do 
  cf <- LSP.getConfig
  when (logOptionsHighLevel $ cfLogOptions cf) $
    logMessage $ T.pack $ "Start operation: " ++ operation
  before <- liftIO getCurrentTime
  res <- runReaderT action (LspContext { ctOperation = operation })
  after <- liftIO getCurrentTime
  when (logOptionsHighLevel (cfLogOptions cf) || logOptionsPerformance (cfLogOptions cf)) $
    logMessage $ T.pack $ "Operation " ++ operation ++ " took: " ++ show (diffUTCTime after before)
  return res

withConnection :: (DbConn -> LspMonad ()) -> LspMonad ()
withConnection act = do
  config <- LSP.getConfig
  conn <- liftIO $ readMVar $ cfConnection config 
  act $ DbConn (cfLogOptions config) conn

-- TODO: save the thread id to let the user cancel them
newRequestThread :: (LSP.MessageParams m -> (Either LSP.ResponseError (LSP.ResponseResult m) -> Lsp ()) -> Lsp ()) -> LSP.RequestMessage m -> (Either LSP.ResponseError (LSP.ResponseResult m) -> Lsp ()) -> Lsp ()
newRequestThread f (LSP.RequestMessage _ _id _ params) responder = do
  isThreaded <- cfIsThreaded <$> LSP.getConfig
  (if isThreaded then forkLsp else id) $ f params responder

newNotificationThread :: (LSP.MessageParams m -> Lsp ()) -> LSP.NotificationMessage m -> Lsp ()
newNotificationThread f (LSP.NotificationMessage _ _ params) = do
  isThreaded <- cfIsThreaded <$> LSP.getConfig
  (if isThreaded then forkLsp else id) $ f params

ensureFileLocationRequest :: LSP.Uri -> (Either LSP.ResponseError a -> LspM Config ()) -> (FilePath -> LspMonad ()) -> LspMonad ()
ensureFileLocationRequest location responder action = case LSP.uriToFilePath location of
  Just fp -> action fp
  Nothing -> do
    operation <- asks ctOperation
    liftLSP $ responder $ Left $ responseError $ T.pack $ "Can't " ++ operation ++ ": Document is not a file"

ensureFileLocation :: LSP.Uri -> (FilePath -> LspMonad ()) -> LspMonad ()
ensureFileLocation location action = case LSP.uriToFilePath location of
  Just fp -> action fp
  Nothing -> do
    operation <- asks ctOperation
    sendError $ T.pack $ "Can't " ++ operation ++ ": Document is not a file"

responseError m = LSP.ResponseError LSP.InvalidRequest m Nothing

lineToLoc :: (String, Int, Int, Int, Int) -> LspMonad (Maybe LSP.Location)
lineToLoc (file, startLine, startCol, endLine, endCol) = do
  rewrites <- getRewrites file 
  return
    $ fmap (LSP.Location (LSP.filePathToUri file) . rangeToLSP)
    $ originalToNewRangeOptimist rewrites 
    $ Range (SP startLine startCol) (SP endLine endCol)

getRewrites :: FilePath -> LspMonad (SourceDiffs Original Modified)
getRewrites fp = do
  cfg <- liftLSP LSP.getConfig
  fileRecords <- liftIO $ readMVar $ cfFileRecords cfg
  return $ maybe emptyDiffs frDiffs $ Map.lookup fp fileRecords

sendMessage :: T.Text -> LspMonad ()
sendMessage = liftLSP . sendNotification LSP.SWindowShowMessage . LSP.ShowMessageParams LSP.MtInfo

sendError :: T.Text -> LspMonad ()
sendError = liftLSP . sendNotification LSP.SWindowShowMessage . LSP.ShowMessageParams LSP.MtError

logMessage :: T.Text -> Lsp ()
logMessage = sendNotification LSP.SWindowLogMessage . LSP.LogMessageParams LSP.MtLog

forkLsp :: MonadUnliftIO m => m () -> m ()
forkLsp action = do
  rio <- askRunInIO
  void $ liftIO $ forkIO $ rio action

handleErrorsCtx :: Connection -> LspMonad () -> LspMonad ()
handleErrorsCtx conn action = do
  operation <- asks ctOperation
  lspHandleErrors conn operation action

lspHandleErrors :: Connection -> String -> LspMonad () -> LspMonad ()
lspHandleErrors conn ctx = flip catch $ \e -> do
  time <- liftIO getCurrentTime
  sendError $ T.pack $ "Error during " ++ ctx ++ ": " ++ show e 
  liftIO $ logErrorMessage conn time ctx (show (e :: SomeException))

instance DBMonad (LspT Config IO) where
  getConnection = cfConnection <$> LSP.getConfig >>= liftIO . readMVar
  logOperation msg = do 
    logOptions <- cfLogOptions <$> LSP.getConfig
    let isLogging = logOptionsPerformance logOptions || logOptionsQueries logOptions
    when isLogging $ logMessage $ T.pack msg
  logPerformance msg = do 
    isLogging <- logOptionsQueries . cfLogOptions <$> LSP.getConfig
    when isLogging $ logMessage $ T.pack msg
  shouldLogFullData = logOptionsFullData . cfLogOptions <$> LSP.getConfig

instance DBMonad LspMonad where
  getConnection = lift getConnection
  logOperation = lift . logOperation
  logPerformance = lift . logPerformance
  shouldLogFullData = lift shouldLogFullData

instance DBMonad (ReaderT DbConn LspMonad) where
  getConnection = asks dbConnConnection
  logOperation msg = do 
    logOptions <- asks dbConnLogOptions
    let isLogging = logOptionsPerformance logOptions || logOptionsQueries logOptions
    when isLogging $ lift $ lift $ logMessage $ T.pack msg
  logPerformance msg = do 
    isLogging <- asks $ logOptionsQueries . dbConnLogOptions
    when isLogging $ lift $ lift $ logMessage $ T.pack msg
  shouldLogFullData = asks $ logOptionsFullData . dbConnLogOptions

