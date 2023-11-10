module Language.Haskell.HsTools.Utils where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.String
import Data.Time.Clock
import Database.PostgreSQL.Simple
import System.Directory
import System.FilePath

-- | Logging options, common for plugin and server
data LogOptions = LogOptions
  { logOptionsHighLevel :: Bool
  , logOptionsQueries :: Bool
  , logOptionsPerformance :: Bool
  , logOptionsFullData :: Bool
  , logOptionsOutputFile :: Maybe FilePath
  }
  deriving (Eq, Ord, Show, Read)

defaultLogOptions = LogOptions
  { logOptionsHighLevel = False
  , logOptionsQueries = False
  , logOptionsPerformance = False
  , logOptionsFullData = False
  , logOptionsOutputFile = Nothing
  }

-- | Everything we need for running database operations
data DbConn = DbConn
  { dbConnLogOptions :: LogOptions
  , dbConnConnection :: Connection
  }

-- | Create a logger function from log options
createLogger :: LogOptions -> (String -> IO ())
createLogger _logOptions = const (return ())
  -- (case logOptionsOutputFile logOptions of
  --   Just logFile -> appendFile logFile
  --   Nothing -> putStrLn) . (++ "\n")

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

withTestFile :: FilePath -> String -> IO a -> IO a
withTestFile fileName content action = (writeFileSafe fileName content >> action) `finally` removeFile fileName

withTestFileLines :: FilePath -> [String] -> IO a -> IO a
withTestFileLines fileName content action = (writeFileSafe fileName (concatMap (++ "\n") content) >> action) `finally` removeFile fileName

writeFileSafe :: FilePath -> String -> IO ()
writeFileSafe filePath content = do
  createDirectoryIfMissing True (takeDirectory filePath)
  writeFile filePath content

withTestRepo :: String -> String -> (DbConn -> IO ()) -> IO ()
withTestRepo baseConnString dbName test = do 
  conn <- connectPostgreSQL (BS.pack $ baseConnString ++ "/postgres")
  createAndRun conn `finally` (execute_ conn (fromString $ "DROP DATABASE IF EXISTS " ++ dbName))
  where
    createAndRun conn = void $ do
      execute_ conn (fromString $ "CREATE DATABASE " ++ dbName) 
      testConn <- connectPostgreSQL (BS.pack $ baseConnString ++ "/" ++ dbName)
      test (DbConn defaultLogOptions testConn)
      close testConn
