module Language.Haskell.HsTools.LspServer.Monad where

import Control.Concurrent.MVar
import Control.Monad.Catch
import Control.Monad.Reader
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

type LspMonad = ReaderT LspContext (LspM Config)

liftLSP :: LspM Config a -> LspMonad a
liftLSP = lift

runInContext :: String -> LspMonad a -> LspM Config a
runInContext operation action = do 
  cf <- LSP.getConfig
  when (logOptionsHighLevel $ cfLogOptions cf) $
    liftIO $ createLogger (cfLogOptions cf) $ "Start operation: " ++ operation ++ "\n"
  runReaderT action (LspContext { ctOperation = operation })

withConnection :: (DbConn -> LspMonad ()) -> LspMonad ()
withConnection act = do
  operation <- asks ctOperation
  conn <- getDbConn
  case conn of
    Just conn -> act conn
    Nothing -> sendError $ T.pack $ operation ++ " needs DB connection"

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

getDbConn :: LspMonad (Maybe DbConn)
getDbConn = do 
  config <- LSP.getConfig
  return $ fmap (DbConn $ cfLogOptions config) $ cfConnection config

handleErrorsCtx :: Connection -> LspMonad () -> LspMonad ()
handleErrorsCtx conn action = do
  operation <- asks ctOperation
  lspHandleErrors conn operation action

lspHandleErrors :: Connection -> String -> LspMonad () -> LspMonad ()
lspHandleErrors conn ctx = flip catch $ \e -> do
  time <- liftIO getCurrentTime
  sendError $ T.pack $ "Error during " ++ ctx ++ ": " ++ show e 
  liftIO $ logErrorMessage conn time ctx (show (e :: SomeException))

