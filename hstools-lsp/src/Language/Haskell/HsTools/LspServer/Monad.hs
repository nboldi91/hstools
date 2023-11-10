{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}

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
import Language.Haskell.HsTools.Utils (LogOptions(..), DbConn(..), createLogger)

data LspContext = LspContext { ctOperation :: String }

type Lsp = LspM Config

type LspMonad = ReaderT LspContext Lsp

liftLSP :: LspM Config a -> LspMonad a
liftLSP = lift

runInContext :: String -> LspMonad a -> LspM Config a
runInContext operation action = do 
  cf <- LSP.getConfig
  when (logOptionsHighLevel $ cfLogOptions cf) $
    liftIO $ createLogger (cfLogOptions cf) $ "Start operation: " ++ operation
  before <- liftIO getCurrentTime
  res <- runReaderT action (LspContext { ctOperation = operation })
  after <- liftIO getCurrentTime
  when (logOptionsHighLevel (cfLogOptions cf) || logOptionsPerformance (cfLogOptions cf)) $
    liftIO $ createLogger (cfLogOptions cf) $ "Operation took: " ++ show (diffUTCTime after before)
  return res

withConnection :: (DbConn -> LspMonad ()) -> LspMonad ()
withConnection act = do
  config <- LSP.getConfig
  conn <- liftIO $ readMVar $ cfConnection config 
  act $ DbConn (cfLogOptions config) conn

-- TODO: save the thread id to let the user cancel them
newRequestThread :: (LSP.MessageParams m -> (Either LSP.ResponseError (LSP.ResponseResult m) -> Lsp ()) -> Lsp ()) -> LSP.RequestMessage m -> (Either LSP.ResponseError (LSP.ResponseResult m) -> Lsp ()) -> Lsp ()
newRequestThread f (LSP.RequestMessage _ _id _ params) responder = forkLsp $ f params responder

newNotificationThread :: (LSP.MessageParams m -> Lsp ()) -> LSP.NotificationMessage m -> Lsp ()
newNotificationThread f (LSP.NotificationMessage _ _ params) = forkLsp $ f params

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

logMessage :: T.Text -> LspMonad ()
logMessage = liftLSP . sendNotification LSP.SWindowLogMessage . LSP.LogMessageParams LSP.MtInfo

forkLsp :: MonadUnliftIO m => m () -> m ()
forkLsp action = do
  rio <- askRunInIO
  void $ liftIO $ forkIO $ rio action

-- | Lets the client know that an async progress was started and reports every part of the task finishing.
-- The actual work is done in an async manner, with the after function being invoked on the final results.
-- wrapWithProgress :: T.Text -> (a -> T.Text) -> (a -> LspMonad b) -> [a] -> ([b] -> LspMonad ()) -> LspMonad ()
-- wrapWithProgress title elemTitle f elems after = do
--   cf <- LSP.getConfig
--   let token = LSP.ProgressNumericToken $ fromIntegral $ cfProcessToken cf
--   rio <- askRunInIO 
--   threadId <- liftIO $ forkIO $ void $ rio $ sendRequest LSP.SWindowWorkDoneProgressCreate (LSP.WorkDoneProgressCreateParams token) $ \_ -> do
--     sendNotification LSP.SProgress $ LSP.ProgressParams token $ LSP.Begin $ LSP.WorkDoneProgressBeginParams title (Just True) (Just $ title <> " starting") (Just 0)
--     allResults <- forM ([1..] `zip` elems) $ \(i, elem) -> do
--       sendNotification LSP.SProgress $ LSP.ProgressParams token $ LSP.Report $ LSP.WorkDoneProgressReportParams (Just True) (Just $ (T.pack $ show i) <> "/" <> (T.pack $ show $ length elems) <> " (" <> elemTitle elem <> ")") (Just $ round $ 100.0 * ((fromIntegral i) / (fromIntegral (length elems))))
--       result <- f elem
--       return result
--     sendNotification LSP.SProgress $ LSP.ProgressParams token $ LSP.End $ LSP.WorkDoneProgressEndParams $ Just (title <> " done")
--     after allResults
--   LSP.setConfig $ cf { cfProcessToken = cfProcessToken cf + 1, cfProcesses = Map.insert token threadId $ cfProcesses cf  }

handleErrorsCtx :: Connection -> LspMonad () -> LspMonad ()
handleErrorsCtx conn action = do
  operation <- asks ctOperation
  lspHandleErrors conn operation action

lspHandleErrors :: Connection -> String -> LspMonad () -> LspMonad ()
lspHandleErrors conn ctx = flip catch $ \e -> do
  time <- liftIO getCurrentTime
  sendError $ T.pack $ "Error during " ++ ctx ++ ": " ++ show e 
  liftIO $ logErrorMessage conn time ctx (show (e :: SomeException))

