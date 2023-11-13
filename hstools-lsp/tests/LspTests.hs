
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module LspTests ( htf_thisModulesTests ) where

import qualified Data.Text as T
import Control.Applicative.Combinators (skipManyTill)
import Control.Concurrent (forkIO)
import System.Process (createPipe)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
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

import Debug.Trace

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
serverConfig = A.Object (A.singleton "hstools" $ A.Object $ A.fromList
  [ ("postgresqlConnectionString", A.String (T.pack connectionString))
  , ("isThreaded", A.Bool False) -- Run the server without threading. The test run is not a normal user.
  ])

useTestRepo :: (ReaderT DbConn IO ()) -> IO ()
useTestRepo test = withTestRepo connectionStringWithoutDB connectionDBName (runReaderT test)

test_simpleGotoDefinition :: Assertion
test_simpleGotoDefinition = useTestRepo $ do
  initializeTables
  (fileName, fileContent) <- setupSimpleTestFile
  liftIO $ runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    definition <- getDefinitions doc (Position 0 5)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 1 0) (Position 1 1)]) definition

test_multiFileGotoDefinition :: Assertion
test_multiFileGotoDefinition = useTestRepo $ do 
  initializeTables
  (fileName, fileContent) <- setupSimpleTestFile
  (fileName2, fileContent2) <- setupAnotherTestFile
  liftIO $ runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    doc2 <- createDoc fileName2 "haskell" fileContent2
    definition <- getDefinitions doc2 (Position 1 5)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 0 0) (Position 0 1)]) definition

test_hovering :: Assertion
test_hovering = useTestRepo $ do 
  initializeTables
  (fileName, fileContent) <- setupSimpleTestFile
  liftIO $ runTest $ do
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    hover <- getHover doc (Position 0 5)
    liftIO $ assertEqual (Just (Hover (HoverContents $ MarkupContent MkMarkdown "\n```hstools\nX.y\n  :: ()\n-- ^ comment for y\n```\n") $ Just $ LSP.Range (Position 0 4) (Position 0 5))) hover

-- file changed during session in an open editor
test_rewriteInEditor :: Assertion
test_rewriteInEditor = useTestRepo $ do 
  initializeTables
  (fileName, fileContent) <- setupSimpleTestFile
  liftIO $ runTest $ do
    skipManyTill anyMessage (customNotification "ChangeFileStates")
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    changeDoc doc [TextDocumentContentChangeEvent (Just $ LSP.Range (Position 0 0) (Position 0 0)) Nothing "\n   "]
    skipManyTill anyMessage (customNotification "ChangeFileStates")
    definition <- getDefinitions doc (Position 1 8)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 2 0) (Position 2 1)]) definition

-- file changed during session in an open editor
test_rewriteInAnotherFile :: Assertion
test_rewriteInAnotherFile = useTestRepo $ do 
  initializeTables
  (fileName, fileContent) <- setupSimpleTestFile
  (anotherFileName, anotherFileContent) <- setupAnotherTestFile
  liftIO $ runTest $ do
    skipManyTill anyMessage (customNotification "ChangeFileStates")
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    doc2@(TextDocumentIdentifier _) <- createDoc anotherFileName "haskell" anotherFileContent
    definition <- getDefinitions doc2 (Position 1 4)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 0 0) (Position 0 1)]) definition
    changeDoc doc [TextDocumentContentChangeEvent (Just $ LSP.Range (Position 0 0) (Position 0 0)) Nothing "\n"]
    skipManyTill anyMessage (customNotification "ChangeFileStates")
    definition2 <- getDefinitions doc2 (Position 1 4)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 1 0) (Position 1 1)]) definition2

test_multipleRewritesInEditor :: Assertion
test_multipleRewritesInEditor = useTestRepo $ do 
  initializeTables
  (fileName, fileContent) <- setupSimpleTestFile
  liftIO $ runTest $ do
    skipManyTill anyMessage (customNotification "ChangeFileStates")
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    changeDoc doc [TextDocumentContentChangeEvent (Just $ LSP.Range (Position 0 0) (Position 0 0)) Nothing "\n"]
    skipManyTill anyMessage (customNotification "ChangeFileStates")
    changeDoc doc [TextDocumentContentChangeEvent (Just $ LSP.Range (Position 0 0) (Position 0 0)) Nothing "\n"]
    skipManyTill anyMessage (customNotification "ChangeFileStates")
    changeDoc doc [TextDocumentContentChangeEvent (Just $ LSP.Range (Position 0 0) (Position 0 0)) Nothing "\n"]
    skipManyTill anyMessage (customNotification "ChangeFileStates")
    definition <- getDefinitions doc (Position 3 5)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 4 0) (Position 4 1)]) definition

-- the file was modified in an earlier session
test_rewriteRecorded :: Assertion
test_rewriteRecorded = useTestRepo $ do 
  initializeTables
  (fileName, fileContent) <- setupSimpleTestFile
  fullFilePath <- liftIO $ ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  time <- liftIO getCurrentTime
  updateFileDiffs fullFilePath time $ Just "1:1-1:1 -> 1:1-1:4"
  liftIO $ runTest $ do
    skipManyTill anyMessage (customNotification "ChangeFileStates")
    doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
    definition <- getDefinitions doc (Position 0 8)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 1 0) (Position 1 1)]) definition

-- the file was modified during the session while it is closed
test_rewriteListener :: Assertion
test_rewriteListener = useTestRepo $ do 
  initializeTables
  (fileName, fileContent) <- setupSimpleTestFile
  fullFilePath <- liftIO $ ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  liftIO $ withTestFile fullFilePath (T.unpack fileContent) $ do
    runTest $ do
      doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
      closeDoc doc
      definition1 <- getDefinitions doc (Position 0 5)
      liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 1 0) (Position 1 1)]) definition1
      liftIO $ writeFile fullFilePath (unlines ["   x = y", "y = ()"])
      sendNotification SWorkspaceDidChangeWatchedFiles $ DidChangeWatchedFilesParams $ List [ FileEvent uri FcChanged ]
      definition2 <- getDefinitions doc (Position 0 8)
      liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 1 0) (Position 1 1)]) definition2

-- the file was modified before the current session
test_rewriteSaved :: Assertion
test_rewriteSaved = useTestRepo $ do 
  initializeTables
  (fileName, fileContent) <- setupSimpleTestFile
  fullFilePath <- liftIO $ ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  liftIO $ withTestFile fullFilePath (unlines ["   x = y", "y = ()"]) $ do
    currentTime <- getCurrentTime
    setModificationTime fullFilePath (addUTCTime 1 currentTime)
    runTest $ do
      doc@(TextDocumentIdentifier uri) <- createDoc fileName "haskell" fileContent
      definition <- getDefinitions doc (Position 0 8)
      liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 1 0) (Position 1 1)]) definition

-- the file was modified during the current session, but we did not get notified
test_didNotNotify :: Assertion
test_didNotNotify = useTestRepo $ do 
  initializeTables
  (fileName, fileContent) <- setupSimpleTestFile
  fullFilePath <- liftIO $ ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  liftIO $ withTestFile fullFilePath (unlines ["x = y", "y = ()"]) $ runTest $ do
    doc@(TextDocumentIdentifier uri) <- openDoc fileName "haskell"
    definition <- getDefinitions doc (Position 0 5)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 1 0) (Position 1 1)]) definition
    closeDoc doc

    liftIO $ writeFile fullFilePath (unlines ["x = y", "", "y = ()"])   
    currentTime <- liftIO getCurrentTime
    liftIO $ setModificationTime fullFilePath (addUTCTime 1 currentTime)

    doc@(TextDocumentIdentifier uri) <- openDoc fileName "haskell"
    definition <- getDefinitions doc (Position 0 5)
    liftIO $ assertEqual (InL [Location uri $ LSP.Range (Position 2 0) (Position 2 1)]) definition

------------------------------------------------------------------------------------------------

setupSimpleTestFile :: ReaderT DbConn IO (FilePath, T.Text)
setupSimpleTestFile = do
  let fileName = testFilePrefix ++ "/X.hs"
  fullFilePath <- liftIO $ ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  let content = unlines ["x = y", "y = ()", "-- ^ comment for y"]
  time <- liftIO getCurrentTime
  mi <- insertModule fullFilePath time "X" "test" content
  asts <- persistAst
    [ FullRange mi $ SP.Range (SP 1 1) (SP 1 2)
    , FullRange mi $ SP.Range (SP 1 5) (SP 1 6)
    , FullRange mi $ SP.Range (SP 2 1) (SP 2 2)
    , FullRange mi $ SP.Range (SP 1 1) (SP 1 6)
    , FullRange mi $ SP.Range (SP 2 1) (SP 2 7)
    , FullRange mi $ SP.Range (SP 1 1) (SP 3 1)
    ]
  defs <- persistDefinitions
    [ (mi, asts !! 5, DefModule)
    , (mi, asts !! 3, DefValue)
    , (mi, asts !! 4, DefValue)
    ]
  persistComments [ (mi, defs !! 2, "-- ^ comment for y") ]
  let x = FullName "X.x" Nothing (Just ValNS)
  let y = FullName "X.y" Nothing (Just ValNS)
  persistName 
    [ (mi, asts !! 0, x, True, Just (SP.Range (SP 1 1) (SP 1 6)))
    , (mi, asts !! 1, y, False, Nothing)
    , (mi, asts !! 2, y, True, Just (SP.Range (SP 2 1) (SP 2 7)))
    ]
  persistTypes
    [ (x, "()")
    , (y, "()")
    ]
  return (fileName, T.pack content)

setupAnotherTestFile :: ReaderT DbConn IO (FilePath, T.Text)
setupAnotherTestFile = do
  let fileName = testFilePrefix ++ "/Y.hs"
  fullFilePath <- liftIO $ ((</> fileName) <$> getCurrentDirectory) >>= canonicalizePath
  let content = unlines ["import X", "z = x"]
  time <- liftIO getCurrentTime
  mi <- insertModule fullFilePath time "Y" "test" content
  asts <- persistAst
    [ FullRange mi $ SP.Range (SP 2 1) (SP 2 2)
    , FullRange mi $ SP.Range (SP 2 5) (SP 2 6)
    , FullRange mi $ SP.Range (SP 2 1) (SP 2 6)
    ]
  let x = FullName "X.x" Nothing (Just ValNS)
  let z = FullName "X.z" Nothing (Just ValNS)
  persistName
    [ (mi, asts !! 0, z, True, Just $ SP.Range (SP 2 1) (SP 2 6))
    , (mi, asts !! 1, x, False, Nothing)
    ]
  persistTypes
    [ (x, "()")
    , (z, "()")
    ]
  return (fileName, T.pack content)

testFilePrefix :: String
testFilePrefix = "hstools-test-temp"

instance DBMonad (ReaderT DbConn IO) where
  getConnection = asks dbConnConnection
  logOperation = const $ return ()
  logPerformance = const $ return ()
  shouldLogFullData = return False
