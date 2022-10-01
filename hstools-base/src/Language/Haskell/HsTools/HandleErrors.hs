module Language.Haskell.HsTools.HandleErrors where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Time.Clock
import Database.PostgreSQL.Simple (Connection)

import Language.Haskell.HsTools.Database

handleErrors :: (MonadCatch m, MonadIO m) => Connection -> String -> m () -> m ()
handleErrors conn ctx = flip catch $ \e -> liftIO $ do
  time <- getCurrentTime
  logErrorMessage conn time ctx (show (e :: SomeException))
