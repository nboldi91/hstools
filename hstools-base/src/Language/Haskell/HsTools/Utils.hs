module Language.Haskell.HsTools.Utils where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.String
import Data.Time.Clock
import Database.PostgreSQL.Simple
import System.Directory
import System.FilePath

readFileContent :: FilePath -> IO (Maybe String)
readFileContent filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then Just <$> readFile filePath
    else return Nothing

getFileModificationTime :: FilePath -> IO (Maybe UTCTime)
getFileModificationTime filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then Just . roundUTCTime <$> getModificationTime filePath
    else return Nothing
 
roundUTCTime :: UTCTime -> UTCTime
roundUTCTime (UTCTime day time) = UTCTime day (picosecondsToDiffTime $ round $ diffTimeToPicoseconds time)
  where round = (* 10^8) . (`div` 10^8)

withTestFile :: FilePath -> String -> IO () -> IO ()
withTestFile fileName content action = (writeFileSafe fileName content >> action) `finally` removeFile fileName

withTestFileLines :: FilePath -> [String] -> IO () -> IO ()
withTestFileLines fileName content action = (writeFileSafe fileName (concatMap (++ "\n") content) >> action) `finally` removeFile fileName

writeFileSafe :: FilePath -> String -> IO ()
writeFileSafe filePath content = do
  createDirectoryIfMissing True (takeDirectory filePath)
  writeFile filePath content

withTestRepo :: String -> String -> (Connection -> IO ()) -> IO ()
withTestRepo baseConnString dbName test = do 
  conn <- connectPostgreSQL (BS.pack $ baseConnString ++ "/postgres")
  createAndRun conn `finally` (execute_ conn (fromString $ "DROP DATABASE IF EXISTS " ++ dbName))
  where
    createAndRun conn = void $ do
      execute_ conn (fromString $ "CREATE DATABASE " ++ dbName) 
      testConn <- connectPostgreSQL (BS.pack $ baseConnString ++ "/" ++ dbName)
      test testConn
      close testConn
