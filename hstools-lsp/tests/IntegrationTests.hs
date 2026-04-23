
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module IntegrationTests ( htf_thisModulesTests ) where

import qualified Data.Text as T
import Control.Applicative.Combinators (skipManyTill)
import Control.Concurrent (forkIO)
import System.Process (createPipe, readProcessWithExitCode)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List (isInfixOf)
import Data.Time.Clock
import Data.String
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Vector as V
import Database.PostgreSQL.Simple
import System.Directory
import System.FilePath
import System.Exit (ExitCode(..))

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

-- | Run the GHC 8.6 plugin on a list of source files to populate the database.
-- The plugin project must be built beforehand (stack build in hstools/).
runPluginOnFiles :: String -> [FilePath] -> IO ()
runPluginOnFiles connStr files = do
  let pluginDir = ".." </> "hstools"
  forM_ files $ \file -> do
    (exitCode, _stdout, stderr) <- readProcessWithExitCode "stack"
      [ "exec", "--stack-yaml", pluginDir </> "stack.yaml", "--"
      , "ghc", "-fno-code"
      , "-fplugin=Language.Haskell.HsTools.Plugin"
      , "-fplugin-opt=Language.Haskell.HsTools.Plugin:" ++ connStr
      , file
      ] ""
    case exitCode of
      ExitSuccess -> return ()
      ExitFailure code -> error $ "Plugin compilation failed for " ++ file
        ++ " (exit code " ++ show code ++ "): " ++ stderr

-- | Write test Haskell source files and return their absolute paths.
writeTestFiles :: FilePath -> [(String, String)] -> IO [FilePath]
writeTestFiles dir fileContents = forM fileContents $ \(name, content) -> do
  let path = dir </> name
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path content
  return path

-- | Clean up test files
cleanTestFiles :: [FilePath] -> IO ()
cleanTestFiles = mapM_ $ \f -> removeFile f `catch` (\(_ :: SomeException) -> return ())

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
connectionDBName = "integrationtestrepo"

serverConfig :: A.Value
serverConfig = A.Object (A.singleton "hstools" $ A.Object $ A.fromList
  [ ("postgresqlConnectionString", A.String (T.pack connectionString))
  , ("isThreaded", A.Bool False)
  ])

useTestRepo :: (ReaderT DbConn IO ()) -> IO ()
useTestRepo test = withTestRepo connectionStringWithoutDB connectionDBName (runReaderT test)

-- | Run the plugin on test files, then run assertions against the DB and LSP server.
withPluginPopulatedDB :: [(String, String)] -> (FilePath -> ReaderT DbConn IO ()) -> IO ()
withPluginPopulatedDB fileContents test = useTestRepo $ do
  initializeTables
  tmpDir <- liftIO $ do
    dir <- canonicalizePath "hstools-integration-test-temp"
    createDirectoryIfMissing True dir
    return dir
  liftIO $ bracket
    (writeTestFiles tmpDir fileContents)
    (\files -> cleanTestFiles files >> removeDirectoryRecursive tmpDir `catch` (\(e :: SomeException) -> return ()))
    (\files -> do
      runPluginOnFiles connectionString files
      -- re-enter ReaderT DbConn IO for the assertions
      return ()
    )
  test tmpDir

-----------------------------------------
--- Integration test: unused instance detection

test_findUnusedInstances_allUsed :: Assertion
test_findUnusedInstances_allUsed = withPluginPopulatedDB
  [ ("M.hs", unlines
      [ "module M where"
      , "data X = X"
      , "instance Show X where"
      , "  show X = \"x\""
      , "y :: String"
      , "y = show X"
      ])
  ] $ \_tmpDir -> do
  unused <- getUnusedInstances
  liftIO $ assertBoolVerbose
    ("Expected no unused instances but got: " ++ show unused)
    (null unused)

test_findUnusedInstances_someUnused :: Assertion
test_findUnusedInstances_someUnused = withPluginPopulatedDB
  [ ("N.hs", unlines
      [ "module N where"
      , "data Y = Y"
      , "instance Show Y where"
      , "  show Y = \"y\""
      , "z :: Int"
      , "z = 42"
      ])
  ] $ \_tmpDir -> do
  instances <- getAllInstances
  usages <- getInstanceUsages
  unused <- getUnusedInstances
  -- Show Y is defined but never used (no call to show on Y values)
  liftIO $ assertBoolVerbose
    ("Expected unused Show instance but got unused=" ++ show unused
      ++ "\nInstances: " ++ show instances
      ++ "\nUsages: " ++ show usages)
    (any (\(_, _, cls, _, _, _, _, _, _) -> cls == "Show") unused)

test_findUnusedInstances_derived :: Assertion
test_findUnusedInstances_derived = withPluginPopulatedDB
  [ ("P.hs", unlines
      [ "module P where"
      , "data Z = Z deriving (Show, Eq)"
      , "w :: String"
      , "w = show Z"
      ])
  ] $ \_tmpDir -> do
  instances <- getAllInstances
  unused <- getUnusedInstances
  -- Show Z is used (via show Z), Eq Z is unused
  liftIO $ assertBoolVerbose
    ("Expected Eq to be unused but got: " ++ show unused
      ++ "\nInstances: " ++ show instances)
    (any (\(_, _, cls, _, _, _, _, _, _) -> cls == "Eq") unused)
  liftIO $ assertBoolVerbose
    ("Expected Show NOT to be unused but got: " ++ show unused)
    (not $ any (\(_, _, cls, _, _, _, _, _, _) -> cls == "Show") unused)

test_findUnusedInstances_nestedUsed :: Assertion
test_findUnusedInstances_nestedUsed = withPluginPopulatedDB
  [ ("R.hs", unlines
      [ "module R where"
      , "data Inner = Inner deriving (Eq)"
      , "data Outer = Outer Inner deriving (Eq)"
      , "same :: Outer -> Outer -> Bool"
      , "same x y = x == y"
      ])
  ] $ \_tmpDir -> do
  instances <- getAllInstances
  unused <- getUnusedInstances
  -- Using == on Outer requires Eq Outer which depends on Eq Inner
  -- so neither Eq instance should be unused
  liftIO $ assertBoolVerbose
    ("Expected no unused Eq instances but got: " ++ show unused
      ++ "\nInstances: " ++ show instances)
    (not $ any (\(_, _, cls, _, _, _, _, _, _) -> cls == "Eq") unused)

test_findUnusedInstances_nestedUnused :: Assertion
test_findUnusedInstances_nestedUnused = withPluginPopulatedDB
  [ ("S.hs", unlines
      [ "module S where"
      , "data Inner = Inner deriving (Eq)"
      , "data Outer = Outer Inner deriving (Eq)"
      , "x :: Int"
      , "x = 42"
      ])
  ] $ \_tmpDir -> do
  instances <- getAllInstances
  unused <- getUnusedInstances
  -- Neither Eq Inner nor Eq Outer is used, both should be unused
  let unusedClasses = map (\(_, _, cls, typ, _, _, _, _, _) -> (cls, typ)) unused
  liftIO $ assertBoolVerbose
    ("Expected Eq Inner to be unused but got: " ++ show unusedClasses
      ++ "\nInstances: " ++ show instances)
    (("Eq", "Inner") `elem` unusedClasses)
  liftIO $ assertBoolVerbose
    ("Expected Eq Outer to be unused but got: " ++ show unusedClasses
      ++ "\nInstances: " ++ show instances)
    (("Eq", "Outer") `elem` unusedClasses)

-- | A generic function with a typeclass constraint: calling it on a concrete
-- type forces that type's instance to be used.
test_findUnusedInstances_genericFunction :: Assertion
test_findUnusedInstances_genericFunction = withPluginPopulatedDB
  [ ("T.hs", unlines
      [ "module T where"
      , "data Color = Red | Green | Blue deriving (Eq, Show)"
      , "isEqual :: Eq a => a -> a -> Bool"
      , "isEqual x y = x == y"
      , "result :: Bool"
      , "result = isEqual Red Blue"
      ])
  ] $ \_tmpDir -> do
  instances <- getAllInstances
  unused <- getUnusedInstances
  -- isEqual Red Blue forces Eq Color to be used; Show Color is not used
  liftIO $ assertBoolVerbose
    ("Expected Eq NOT to be unused but got: " ++ show unused
      ++ "\nInstances: " ++ show instances)
    (not $ any (\(_, _, cls, _, _, _, _, _, _) -> cls == "Eq") unused)
  liftIO $ assertBoolVerbose
    ("Expected Show to be unused but got: " ++ show unused
      ++ "\nInstances: " ++ show instances)
    (any (\(_, _, cls, _, _, _, _, _, _) -> cls == "Show") unused)

-- | A generic data type where the call-site constraint dictates that the
-- inner type must have a certain typeclass.
test_findUnusedInstances_genericDataType :: Assertion
test_findUnusedInstances_genericDataType = withPluginPopulatedDB
  [ ("U.hs", unlines
      [ "module U where"
      , "data Box a = Box a deriving (Show, Eq)"
      , "data Fruit = Apple | Banana deriving (Show, Eq)"
      , "myBox :: Box Fruit"
      , "myBox = Box Apple"
      , "label :: String"
      , "label = show myBox"
      ])
  ] $ \_tmpDir -> do
  instances <- getAllInstances
  unused <- getUnusedInstances
  -- show myBox forces Show (Box Fruit) which requires Show Fruit
  -- Eq Box and Eq Fruit are never used
  let unusedClasses = map (\(_, _, cls, _, _, _, _, _, _) -> cls) unused
  liftIO $ assertBoolVerbose
    ("Expected Show NOT to be unused but got: " ++ show unused
      ++ "\nInstances: " ++ show instances)
    ("Show" `notElem` unusedClasses)
  liftIO $ assertBoolVerbose
    ("Expected Eq to be unused but got: " ++ show unused
      ++ "\nInstances: " ++ show instances)
    ("Eq" `elem` unusedClasses)

test_findUnusedInstances_lspRequest :: Assertion
test_findUnusedInstances_lspRequest = withPluginPopulatedDB
  [ ("Q.hs", unlines
      [ "module Q where"
      , "data W = W deriving (Show, Eq)"
      , "v :: String"
      , "v = show W"
      ])
  ] $ \_tmpDir -> do
  liftIO $ runTest $ do
    -- send the FindUnusedInstances custom request and wait for response
    ResponseMessage _ _ result <- LSP.request (SCustomMethod "FindUnusedInstances") A.Null
    case result of
      Right val -> do
        let arr = case val of
              A.Array v -> V.toList v
              _ -> []
        -- Eq should be unused, Show should not
        let classNames = map extractClassName arr
        liftIO $ assertBoolVerbose
          ("Expected Eq in unused instances but got: " ++ show classNames)
          ("Eq" `elem` classNames)
        liftIO $ assertBoolVerbose
          ("Expected Show NOT in unused instances but got: " ++ show classNames)
          ("Show" `notElem` classNames)
      Left err ->
        liftIO $ assertFailure $ "FindUnusedInstances request failed: " ++ show err

extractClassName :: A.Value -> String
extractClassName (A.Object o) = case A.lookup "className" o of
  Just (A.String t) -> T.unpack t
  _ -> ""
extractClassName _ = ""


instance DBMonad (ReaderT DbConn IO) where
  getConnection = asks dbConnConnection
  logOperation = const $ return ()
  logPerformance = const $ return ()
  shouldLogFullData = return False
