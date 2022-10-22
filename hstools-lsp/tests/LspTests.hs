
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
import Data.String
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Database.PostgreSQL.Simple
import System.Directory
import System.FilePath

import qualified Test.Framework as F
import Test.Framework.HUnitWrapper
import Test.HUnit.Base (Assertion)
import Language.LSP.Test as LSP
import Language.LSP.Types as LSP

import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.LspServer.LspServer (mainWithHandles)
import Language.Haskell.HsTools.SourcePosition as SP
import Language.Haskell.HsTools.SourceDiffs
import Language.Haskell.HsTools.Utils

runTest :: Session () -> IO ()
runTest session = do
  (hinRead, hinWrite) <- createPipe
  (houtRead, houtWrite) <- createPipe
  forkIO $ void $ mainWithHandles hinRead houtWrite
  runSessionWithHandles hinWrite houtRead testConfig fullCaps "." session

testConfig :: SessionConfig
testConfig = defaultConfig { lspConfig = Just serverConfig }

connectionStringWithoutDB :: String
connectionStringWithoutDB = "postgresql://saver:saver@127.0.0.1:5432"

connectionString :: String
connectionString = connectionStringWithoutDB ++ "/" ++ connectionDBName

connectionDBName :: String
connectionDBName = "lsptestrepo"

serverConfig :: A.Value
serverConfig = A.Object (A.singleton "hstools" $ A.Object (A.singleton "postgresqlConnectionString" $ A.String (T.pack connectionString)))

useTestRepo test = withTestRepo connectionStringWithoutDB connectionDBName (\conn -> test conn `finally` printErrors conn)
  where printErrors conn = getErrors conn >>= mapM_ print 

test_simpleGotoDefinition :: Assertion
test_simpleGotoDefinition = useTestRepo $ \conn -> do
  initializeTables conn
  (fileName, fileContent) <- setupSimpleTestFile conn
  runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    definition <- getDefinitions doc (Position 0 5)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 1 0) (Position 1 1)]) definition

test_multiFileGotoDefinition :: Assertion
test_multiFileGotoDefinition = useTestRepo $ \conn -> do 
  initializeTables conn
  (fileName, fileContent) <- setupSimpleTestFile conn
  (fileName2, fileContent2) <- setupAnotherTestFile conn
  runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    doc2 <- createDoc fileName2 "haskell" fileContent2
    definition <- getDefinitions doc2 (Position 1 5)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 0 0) (Position 0 1)]) definition

test_hovering :: Assertion
test_hovering = useTestRepo $ \conn -> do 
  initializeTables conn
  (fileName, fileContent) <- setupSimpleTestFile conn
  runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    hover <- getHover doc (Position 0 5)
    liftIO $ assertEqual (Just (Hover (HoverContents $ MarkupContent MkMarkdown "\n```hstools\nX.y\n  :: ()\n-- ^ comment for y\n```\n") $ Just $ LSP.Range (Position 0 4) (Position 0 5))) hover

-- file changed during session in an open editor
test_rewriteInEditor :: Assertion
test_rewriteInEditor = useTestRepo $ \conn -> do 
  initializeTables conn
  (fileName, fileContent) <- setupSimpleTestFile conn
  runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    changeDoc doc [TextDocumentContentChangeEvent (Just $ LSP.Range (Position 0 0) (Position 0 0)) Nothing "\n   "]
    definition <- getDefinitions doc (Position 1 8)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 2 0) (Position 2 1)]) definition

-- file changed during session in an open editor
test_rewriteInAnotherFile :: Assertion
test_rewriteInAnotherFile = useTestRepo $ \conn -> do 
  initializeTables conn
  (fileName, fileContent) <- setupSimpleTestFile conn
  (anotherFileName, anotherFileContent) <- setupAnotherTestFile conn
  runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    doc2@(TextDocumentIdentifier _) <- createDoc anotherFileName "haskell" anotherFileContent
    definition <- getDefinitions doc2 (Position 1 4)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 0 0) (Position 0 1)]) definition
    changeDoc doc [TextDocumentContentChangeEvent (Just $ LSP.Range (Position 0 0) (Position 0 0)) Nothing "\n"]
    definition2 <- getDefinitions doc2 (Position 1 4)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 1 0) (Position 1 1)]) definition2

test_multipleRewritesInEditor :: Assertion
test_multipleRewritesInEditor = useTestRepo $ \conn -> do 
  initializeTables conn
  (fileName, fileContent) <- setupSimpleTestFile conn
  runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    changeDoc doc [TextDocumentContentChangeEvent (Just $ LSP.Range (Position 0 0) (Position 0 0)) Nothing "\n"]
    changeDoc doc [TextDocumentContentChangeEvent (Just $ LSP.Range (Position 0 0) (Position 0 0)) Nothing "\n"]
    changeDoc doc [TextDocumentContentChangeEvent (Just $ LSP.Range (Position 0 0) (Position 0 0)) Nothing "\n"]
    definition <- getDefinitions doc (Position 3 5)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 4 0) (Position 4 1)]) definition

-- the file was modified in an earlier session
test_rewriteRecorded :: Assertion
test_rewriteRecorded = useTestRepo $ \conn -> do 
  initializeTables conn
  (fileName, fileContent) <- setupSimpleTestFile conn
  fullFilePath <- ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  time <- getCurrentTime
  updateFileDiffs conn fullFilePath time $ Just "1:1-1:1 -> 1:1-1:4"
  runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    definition <- getDefinitions doc (Position 0 8)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 1 0) (Position 1 1)]) definition

-- the file was modified during the session while it is closed
test_rewriteListener :: Assertion
test_rewriteListener = useTestRepo $ \conn -> do 
  initializeTables conn
  (fileName, fileContent) <- setupSimpleTestFile conn
  fullFilePath <- ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  withTestFile fullFilePath (T.unpack fileContent) $ do
    runTest $ do
      liftIO $ writeFile fullFilePath (unlines ["   x = y", "y = ()"])
      doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
      sendNotification SWorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $ List [ FileEvent uri FcChanged ]
      definition <- getDefinitions doc (Position 0 8)
      liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 1 0) (Position 1 1)]) definition

-- the file was modified before the current session
test_rewriteSaved :: Assertion
test_rewriteSaved = useTestRepo $ \conn -> do 
  initializeTables conn
  (fileName, fileContent) <- setupSimpleTestFile conn
  fullFilePath <- ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  withTestFile fullFilePath (unlines ["   x = y", "y = ()"]) $ do
    currentTime <- getCurrentTime
    setModificationTime fullFilePath (addUTCTime 1 currentTime)
    runTest $ do
      doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
      definition <- getDefinitions doc (Position 0 8)
      liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 1 0) (Position 1 1)]) definition

------------------------------------------------------------------------------------------------

setupSimpleTestFile :: Connection -> IO (FilePath, T.Text)
setupSimpleTestFile conn = do
  let fileName = testFilePrefix ++ "/X.hs"
  fullFilePath <- ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  let content = unlines ["x = y", "y = ()", "-- ^ comment for y"]
  time <- getCurrentTime
  mi <- insertModule conn fullFilePath time "X" "test" content
  asts <- persistAst conn
    [ (mi, 1, 1, 1, 2)
    , (mi, 1, 5, 1, 6)
    , (mi, 2, 1, 2, 2)
    , (mi, 1, 1, 1, 6)
    , (mi, 2, 1, 2, 7)
    , (mi, 1, 1, 3, 1)
    ]
  defs <- persistDefinitions conn
    [ (mi, asts !! 5, DefModule)
    , (mi, asts !! 3, DefValue)
    , (mi, asts !! 4, DefValue)
    ]
  persistComments conn [ (mi, defs !! 2, "-- ^ comment for y") ]
  persistName conn 
    [ (mi, asts !! 0, "X.x", vnms, True, Just 1, Just 1, Just 1, Just 6)
    , (mi, asts !! 1, "X.y", vnms, False, Nothing, Nothing, Nothing, Nothing)
    , (mi, asts !! 2, "X.y", vnms, True, Just 2, Just 1, Just 2, Just 7)
    ]
  persistTypes conn
    [ ("X.x", vnms, "()")
    , ("X.y", vnms, "()")
    ]
  return (fileName, T.pack content)

setupAnotherTestFile :: Connection -> IO (FilePath, T.Text)
setupAnotherTestFile conn = do
  let fileName = testFilePrefix ++ "/Y.hs"
  fullFilePath <- ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  let content = unlines ["import X", "z = x"]
  time <- getCurrentTime
  mi <- insertModule conn fullFilePath time "Y" "test" content
  asts <- persistAst conn
    [ (mi, 2, 1, 2, 2)
    , (mi, 2, 5, 2, 6)
    , (mi, 2, 1, 2, 6)
    ]
  persistName conn
    [ (mi, asts !! 0, "Y.z", vnms, True, Just 2, Just 1, Just 2, Just 6)
    , (mi, asts !! 1, "X.x", vnms, False, Nothing, Nothing, Nothing, Nothing)
    ]
  persistTypes conn
    [ ("X.x", vnms, "()")
    , ("X.z", vnms, "()")
    ]
  return (fileName, T.pack content)

-- value namespace
vnms :: Maybe Int
vnms = Just (fromEnum ValNS)

testFilePrefix :: String
testFilePrefix = "hstools-test-temp"
