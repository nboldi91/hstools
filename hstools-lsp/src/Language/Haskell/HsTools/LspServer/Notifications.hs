{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Language.Haskell.HsTools.LspServer.Notifications where

import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple.Notification as SQL
import Language.LSP.Types as LSP

import Language.Haskell.HsTools.LspServer.Utils
import Language.Haskell.HsTools.LspServer.FileRecords
import Language.Haskell.HsTools.SourceDiffs

-- Listens to the compile process changing the DB when the source is recompiled
handleNotifications :: Connection -> FileRecords -> (FromServerMessage -> IO ()) -> IO ()
handleNotifications conn fileRecords messageHandler = do
  SQL.Notification _pid channel fileName <- SQL.getNotification conn
  when (channel == "module_clean") $ do
    markFileRecordsClean [BS.unpack fileName] fileRecords
    updateFileStatesIO (BS.unpack fileName) fileRecords messageHandler
  handleNotifications conn fileRecords messageHandler

updateFileStatesIO :: FilePath -> FileRecords -> (FromServerMessage -> IO ()) -> IO ()
updateFileStatesIO filePath fileRecords messageHandler = do
  fr <- readMVar fileRecords
  let status = Map.lookup filePath $ Map.map frDiffs fr
  sendFileStatesIO (maybe [] ((:[]) . (filePath,)) status) messageHandler

-- | Does the same as 
sendFileStatesIO :: [(FilePath, SourceDiffs Original Modified)] -> (FromServerMessage -> IO ()) -> IO ()
sendFileStatesIO [] _ = return () 
sendFileStatesIO states messageHandler
  = messageHandler $ FromServerMess changeFileStatesMethod 
      $ NotMess $ NotificationMessage "2.0" changeFileStatesMethod $ createChangeFileStates states

