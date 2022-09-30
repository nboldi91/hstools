
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE OverloadedStrings #-}

module LspTests ( htf_thisModulesTests ) where

import qualified Data.Text as T
import Control.Concurrent (forkIO)
import System.Process (createPipe)
import Control.Monad.IO.Class
import Control.Monad
import Data.Time.Clock
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import System.Directory
import System.FilePath

import qualified Test.Framework as F
import Language.LSP.Test
import Language.LSP.Types

import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.LspServer (mainWithHandles)

assertEqual exp act = liftIO $ F.assertEqual exp act

runTest :: Session () -> IO ()
runTest session = do
  (hinRead, hinWrite) <- createPipe
  (houtRead, houtWrite) <- createPipe
  forkIO $ void $ mainWithHandles hinRead houtWrite
  runSessionWithHandles hinWrite houtRead testConfig fullCaps "." session

testConfig :: SessionConfig
testConfig = defaultConfig { lspConfig = Just serverConfig }

connectionString :: String
connectionString = "postgresql://saver:saver@127.0.0.1:5432/repo"

serverConfig :: A.Value
serverConfig = A.Object (A.singleton "hstools" $ A.Object (A.singleton "postgresqlConnectionString" $ A.String (T.pack connectionString)))


test_simpleGotoDefinition :: IO ()
test_simpleGotoDefinition = do 
  conn <- connectPostgreSQL (BS.pack connectionString)
  cleanModulesFromDB conn testFilePrefix
  (fileName, fileContent) <- setupSimpleTestFile conn
  runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    definition <- getDefinitions doc (Position 0 5)
    assertEqual (InL [Location uri $ Range (Position 1 0) (Position 1 1)]) definition

test_multiFileGotoDefinition :: IO ()
test_multiFileGotoDefinition = do 
  conn <- connectPostgreSQL (BS.pack connectionString)
  cleanModulesFromDB conn testFilePrefix
  (fileName, fileContent) <- setupSimpleTestFile conn
  (fileName2, fileContent2) <- setupAnotherTestFile conn
  runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    doc2 <- createDoc fileName2 "haskell" fileContent2
    definition <- getDefinitions doc2 (Position 1 5)
    assertEqual (InL [Location uri $ Range (Position 0 0) (Position 0 1)]) definition


setupSimpleTestFile :: Connection -> IO (FilePath, T.Text)
setupSimpleTestFile conn = do
  let fileName = testFilePrefix ++ "/X.hs"
  fullFilePath <- ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  let content = unlines ["x = y", "y = 1"]
  time <- getCurrentTime
  mi <- insertModule conn fullFilePath time "X" "test" content
  asts <- persistAst conn [(mi, 1, 1, 1, 2), (mi, 1, 5, 1, 6), (mi, 2, 1, 2, 2)]
  persistName conn [(mi, asts !! 0, "x", vnms, True), (mi, asts !! 1, "y", vnms, False), (mi, asts !! 2, "y", vnms, True)]
  return (fileName, T.pack content)

setupAnotherTestFile :: Connection -> IO (FilePath, T.Text)
setupAnotherTestFile conn = do
  let fileName = testFilePrefix ++ "/Y.hs"
  fullFilePath <- ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  let content = unlines ["import X", "z = x"]
  time <- getCurrentTime
  mi <- insertModule conn fullFilePath time "X" "test" content
  asts <- persistAst conn [(mi, 2, 1, 2, 2), (mi, 2, 5, 2, 6)]
  persistName conn [(mi, asts !! 0, "z", vnms, True), (mi, asts !! 1, "x", vnms, False)]
  return (fileName, T.pack content)

-- value namespace
vnms :: Maybe Int
vnms = Just (fromEnum ValNS)

testFilePrefix :: String
testFilePrefix = "hstools-test"