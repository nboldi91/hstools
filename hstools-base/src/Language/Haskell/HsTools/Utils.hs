module Language.Haskell.HsTools.Utils where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.String
import Data.Time.Clock
import Database.PostgreSQL.Simple
import System.Directory
import System.FilePath

-- | Verbosity level (common option between plugin and server)
data Verbosity = VerbositySilent | VerbosityVerbose | VerbosityDebug
  deriving (Eq, Ord, Show, Read)

-- | Everything we need for running database operations
data DbConn = DbConn { dbConnLogger :: String -> IO (), dbConnConnection :: Connection } 

-- | Logger that logs to standard output (used for plugin) 
debugStdOutLogger :: Verbosity -> (String -> IO ())
debugStdOutLogger verbosity = if verbosity >= VerbosityDebug then putStrLn else const (return ())

-- | Logger that logs to a specified file (used by server)
debugFileLogger :: FilePath -> Verbosity -> (String -> IO ())
debugFileLogger logFile verbosity = if verbosity >= VerbosityDebug then appendFile logFile . (++"\n") else const (return ())

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
      test (DbConn (debugStdOutLogger VerbositySilent) testConn)
      close testConn
