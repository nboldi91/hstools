
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE OverloadedStrings #-}

module LspTests ( htf_thisModulesTests ) where

import qualified Data.Text as T
import Control.Concurrent (forkIO)
import System.Process (createPipe)
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad
import Data.List
import Data.Time.Clock
import qualified Data.Map as Map
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
import Language.Haskell.HsTools.LspServer.LspServer (mainWithHandles)
import Language.Haskell.HsTools.LinesDiff
import Language.Haskell.HsTools.Utils

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

test_hovering :: IO ()
test_hovering = do 
  conn <- connectPostgreSQL (BS.pack connectionString)
  cleanModulesFromDB conn testFilePrefix
  (fileName, fileContent) <- setupSimpleTestFile conn
  runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    hover <- getHover doc (Position 0 5)
    assertEqual (Just (Hover (HoverContents $ MarkupContent MkMarkdown "\n```hstools\ny\n  :: ()\n```\n") $ Just $ Range (Position 0 4) (Position 0 5))) hover

-- file changed during session in an open editor
test_rewriteInEditor :: IO ()
test_rewriteInEditor = do 
  conn <- connectPostgreSQL (BS.pack connectionString)
  cleanModulesFromDB conn testFilePrefix
  (fileName, fileContent) <- setupSimpleTestFile conn
  runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    changeDoc doc [TextDocumentContentChangeEvent (Just $ Range (Position 0 0) (Position 0 0)) Nothing "\n   "]
    definition <- getDefinitions doc (Position 1 8)
    assertEqual (InL [Location uri $ Range (Position 2 0) (Position 2 1)]) definition

test_multipleRewritesInEditor :: IO ()
test_multipleRewritesInEditor = do 
  conn <- connectPostgreSQL (BS.pack connectionString)
  cleanModulesFromDB conn testFilePrefix
  (fileName, fileContent) <- setupSimpleTestFile conn
  runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    changeDoc doc [TextDocumentContentChangeEvent (Just $ Range (Position 0 0) (Position 0 0)) Nothing "\n"]
    changeDoc doc [TextDocumentContentChangeEvent (Just $ Range (Position 0 0) (Position 0 0)) Nothing "\n"]
    changeDoc doc [TextDocumentContentChangeEvent (Just $ Range (Position 0 0) (Position 0 0)) Nothing "\n"]
    definition <- getDefinitions doc (Position 3 5)
    assertEqual (InL [Location uri $ Range (Position 4 0) (Position 4 1)]) definition

-- the file was modified in an earlier session
test_rewriteRecorded :: IO ()
test_rewriteRecorded = do 
  conn <- connectPostgreSQL (BS.pack connectionString)
  cleanModulesFromDB conn testFilePrefix
  (fileName, fileContent) <- setupSimpleTestFile conn
  fullFilePath <- ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  time <- getCurrentTime
  updateFileDiffs conn fullFilePath time $ Just $ serializeSourceDiffs (Map.fromList [(SP 1 1, SourceDiffData (SP 1 1) (DSP 0 3 1))])
  runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    definition <- getDefinitions doc (Position 0 8)
    assertEqual (InL [Location uri $ Range (Position 1 0) (Position 1 1)]) definition

-- the file was modified during the session while it is closed
test_rewriteListener :: IO ()
test_rewriteListener = do 
  conn <- connectPostgreSQL (BS.pack connectionString)
  cleanModulesFromDB conn testFilePrefix
  (fileName, fileContent) <- setupSimpleTestFile conn
  fullFilePath <- ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  withTestFile fullFilePath (T.unpack fileContent) $ do
    runTest $ do
      liftIO $ writeFile fullFilePath (unlines ["   x = y", "y = ()"])
      doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
      sendNotification SWorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $ List [ FileEvent uri FcChanged ]
      definition <- getDefinitions doc (Position 0 8)
      assertEqual (InL [Location uri $ Range (Position 1 0) (Position 1 1)]) definition

-- the file was modified before the current session
test_rewriteSaved :: IO ()
test_rewriteSaved = do 
  conn <- connectPostgreSQL (BS.pack connectionString)
  cleanModulesFromDB conn testFilePrefix
  (fileName, fileContent) <- setupSimpleTestFile conn
  fullFilePath <- ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  withTestFile fullFilePath (unlines ["   x = y", "y = ()"]) $ do
    currentTime <- getCurrentTime
    setModificationTime fullFilePath (addUTCTime 1 currentTime)
    runTest $ do
      doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
      definition <- getDefinitions doc (Position 0 8)
      assertEqual (InL [Location uri $ Range (Position 1 0) (Position 1 1)]) definition

setupSimpleTestFile :: Connection -> IO (FilePath, T.Text)
setupSimpleTestFile conn = do
  let fileName = testFilePrefix ++ "/X.hs"
  fullFilePath <- ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  let content = unlines ["x = y", "y = ()"]
  time <- getCurrentTime
  mi <- insertModule conn fullFilePath time "X" "test" content
  asts <- persistAst conn [(mi, 1, 1, 1, 2), (mi, 1, 5, 1, 6), (mi, 2, 1, 2, 2)]
  persistName conn [(mi, asts !! 0, "x", vnms, True), (mi, asts !! 1, "y", vnms, False), (mi, asts !! 2, "y", vnms, True)]
  persistTypes conn [(mi, asts !! 0, "()"), (mi, asts !! 1, "()"), (mi, asts !! 2, "()")]
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
  persistTypes conn [(mi, asts !! 0, "()"), (mi, asts !! 1, "()")]
  return (fileName, T.pack content)

-- value namespace
vnms :: Maybe Int
vnms = Just (fromEnum ValNS)

testFilePrefix :: String
testFilePrefix = "hstools-test-temp"