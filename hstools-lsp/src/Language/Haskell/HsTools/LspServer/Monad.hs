module Language.Haskell.HsTools.LspServer.Monad where

import Control.Concurrent.MVar
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import Language.LSP.Server as LSP
import Language.LSP.Types as LSP

import Language.Haskell.HsTools.LspServer.State
import Language.Haskell.HsTools.LspServer.FileRecords
import Language.Haskell.HsTools.SourceDiffs
import Language.Haskell.HsTools.HandleErrors

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

ensureFileLocationRequest :: Uri -> (Either ResponseError a -> LspM Config ()) -> (FilePath -> LspMonad ()) -> LspMonad ()
ensureFileLocationRequest location responder action = case uriToFilePath location of
  Just fp -> action fp
  Nothing -> do
    operation <- asks ctOperation
    liftLSP $ responder $ Left $ responseError $ T.pack $ "Can't " ++ operation ++ ": Document is not a file"

ensureFileLocation :: Uri -> (FilePath -> LspMonad ()) -> LspMonad ()
ensureFileLocation location action = case uriToFilePath location of
  Just fp -> action fp
  Nothing -> do
    operation <- asks ctOperation
    sendError $ T.pack $ "Can't " ++ operation ++ ": Document is not a file"

responseError m = ResponseError InvalidRequest m Nothing

getRewrites :: FilePath -> LspMonad (SourceDiffs Original Modified)
getRewrites fp = do
  cfg <- liftLSP LSP.getConfig
  fileRecords <- liftIO $ readMVar $ cfFileRecords cfg
  return $ maybe emptyDiffs frDiffs $ Map.lookup fp fileRecords

sendMessage :: T.Text -> LspMonad ()
sendMessage = liftLSP . sendNotification SWindowShowMessage . ShowMessageParams MtInfo

sendError :: T.Text -> LspMonad ()
sendError = liftLSP . sendNotification SWindowShowMessage . ShowMessageParams MtError

logMessage :: T.Text -> LspMonad ()
logMessage = liftLSP . sendNotification SWindowLogMessage . LogMessageParams MtInfo

handleErrorsCtx :: Connection -> LspMonad () -> LspMonad ()
handleErrorsCtx conn action = do
  operation <- asks ctOperation
  handleErrors conn ("During operation " ++ operation) action
