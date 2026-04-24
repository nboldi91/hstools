
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
runPluginOnFiles :: String -> FilePath -> [FilePath] -> IO ()
runPluginOnFiles connStr baseDir files = do
  let pluginDir = ".." </> "hstools"
  forM_ files $ \file -> do
    (exitCode, _stdout, stderr) <- readProcessWithExitCode "stack"
      [ "exec", "--stack-yaml", pluginDir </> "stack.yaml", "--"
      , "ghc", "-fno-code"
      , "-i" ++ baseDir
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
      runPluginOnFiles connectionString tmpDir files
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

-- | Regression: using a class on one type must not mark all instances of that
-- class as used.  The old query matched only on className, so a single Eq usage
-- on type A would prevent Eq B from being reported as unused.
test_findUnusedInstances_classNotSharedAcrossTypes :: Assertion
test_findUnusedInstances_classNotSharedAcrossTypes = withPluginPopulatedDB
  [ ("V.hs", unlines
      [ "module V where"
      , "data A = A deriving (Eq)"
      , "data B = B deriving (Eq)"
      , "same :: A -> A -> Bool"
      , "same x y = x == y"
      ])
  ] $ \_tmpDir -> do
  unused <- getUnusedInstances
  -- Eq A is used via (==), but Eq B is never used
  let unusedPairs = map (\(_, _, cls, typ, _, _, _, _, _) -> (cls, typ)) unused
  liftIO $ assertBoolVerbose
    ("Expected Eq B to be unused but got: " ++ show unusedPairs)
    (("Eq", "B") `elem` unusedPairs)
  liftIO $ assertBoolVerbose
    ("Expected Eq A NOT to be unused but got: " ++ show unusedPairs)
    (("Eq", "A") `notElem` unusedPairs)

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
  usages <- getInstanceUsages
  deps <- getInstanceDeps
  unused <- getUnusedInstances
  -- Using == on Outer requires Eq Outer which depends on Eq Inner
  -- so neither Eq instance should be unused
  liftIO $ assertBoolVerbose
    ("Expected no unused Eq instances but got: " ++ show unused
      ++ "\nInstances: " ++ show instances
      ++ "\nUsages: " ++ show usages
      ++ "\nDeps: " ++ show deps)
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
  usages <- getInstanceUsages
  unused <- getUnusedInstances
  -- show myBox forces Show (Box Fruit) which requires Show Fruit
  -- Eq Box and Eq Fruit are never used
  let unusedClasses = map (\(_, _, cls, _, _, _, _, _, _) -> cls) unused
  liftIO $ assertBoolVerbose
    ("Expected Show NOT to be unused but got: " ++ show unused
      ++ "\nInstances: " ++ show instances
      ++ "\nUsages: " ++ show usages)
    ("Show" `notElem` unusedClasses)
  liftIO $ assertBoolVerbose
    ("Expected Eq to be unused but got: " ++ show unused
      ++ "\nInstances: " ++ show instances)
    ("Eq" `elem` unusedClasses)

-- | A hand-written instance whose class has a superclass constraint.
-- Using the subclass instance should keep the superclass instance alive.
test_findUnusedInstances_superclassDep :: Assertion
test_findUnusedInstances_superclassDep = withPluginPopulatedDB
  [ ("W.hs", unlines
      [ "module W where"
      , "class Eq a => MyOrd a where"
      , "  cmp :: a -> a -> Bool"
      , "data Rec = Rec Int deriving (Eq)"
      , "instance MyOrd Rec where"
      , "  cmp x y = x == y"
      , "test :: Rec -> Rec -> Bool"
      , "test = cmp"
      ])
  ] $ \_tmpDir -> do
  unused <- getUnusedInstances
  deps <- getInstanceDeps
  let unusedPairs = map (\(_, _, cls, typ, _, _, _, _, _) -> (cls, typ)) unused
  -- MyOrd Rec is used (via cmp), and it requires Eq Rec as superclass
  -- so Eq Rec should NOT be unused
  liftIO $ assertBoolVerbose
    ("Expected MyOrd Rec NOT to be unused but got: " ++ show unusedPairs
      ++ "\nDeps: " ++ show deps)
    (("MyOrd", "Rec") `notElem` unusedPairs)
  liftIO $ assertBoolVerbose
    ("Expected Eq Rec NOT to be unused (superclass of MyOrd) but got: " ++ show unusedPairs
      ++ "\nDeps: " ++ show deps)
    (("Eq", "Rec") `notElem` unusedPairs)

-- | An empty hand-written instance where the class has a default method
-- that requires Show.  Mirrors the DeriveAnyClass Binary+Generic pattern:
-- the empty instance body relies on the default method, which needs another
-- instance.  Using the outer instance should keep the inner one alive.
test_findUnusedInstances_defaultMethodDep :: Assertion
test_findUnusedInstances_defaultMethodDep = withPluginPopulatedDB
  [ ("X.hs", unlines
      [ "{-# LANGUAGE DefaultSignatures #-}"
      , "module X where"
      , "class MySerialize a where"
      , "  encode :: a -> String"
      , "  default encode :: Show a => a -> String"
      , "  encode = show"
      , "data Rec = Rec Int deriving (Show)"
      , "instance MySerialize Rec"
      , "test :: String"
      , "test = encode (Rec 42)"
      ])
  ] $ \_tmpDir -> do
  unused <- getUnusedInstances
  deps <- getInstanceDeps
  let unusedPairs = map (\(_, _, cls, typ, _, _, _, _, _) -> (cls, typ)) unused
  -- MySerialize Rec is used (via encode), its default method needs Show Rec
  -- so Show Rec should NOT be unused
  liftIO $ assertBoolVerbose
    ("Expected MySerialize Rec NOT to be unused but got: " ++ show unusedPairs
      ++ "\nDeps: " ++ show deps)
    (("MySerialize", "Rec") `notElem` unusedPairs)
  liftIO $ assertBoolVerbose
    ("Expected Show Rec NOT to be unused (needed by default encode) but got: " ++ show unusedPairs
      ++ "\nDeps: " ++ show deps)
    (("Show", "Rec") `notElem` unusedPairs)

-- | Derived Data instances where the dependency is in the method body (gfoldl),
-- not in the evidence bindings.  A container type's Data instance references
-- the sub-type's Data instance via the higher-order 'k' argument.
test_findUnusedInstances_dataDerivedDep :: Assertion
test_findUnusedInstances_dataDerivedDep = withPluginPopulatedDB
  [ ("DD.hs", unlines
      [ "{-# LANGUAGE DeriveDataTypeable #-}"
      , "module DD where"
      , "import Data.Data (Data, toConstr)"
      , "data Sub = Sub Int deriving (Data)"
      , "data Container = Container Sub deriving (Data)"
      , "-- Use Data Container via toConstr"
      , "test :: Container -> String"
      , "test x = show (toConstr x)"
      ])
  ] $ \_tmpDir -> do
  unused <- getUnusedInstances
  deps <- getInstanceDeps
  let unusedPairs = map (\(_, _, cls, typ, _, _, _, _, _) -> (cls, typ)) unused
  -- Data Container depends on Data Sub via gfoldl body — verify in deps
  let depPairs = map (\(_, cls, typ) -> (cls, typ)) deps
  liftIO $ assertBoolVerbose
    ("Expected dep (Data, Sub) in instance_deps but got: " ++ show deps)
    (("Data", "Sub") `elem` depPairs)
  -- Data Container is used; it depends on Data Sub via gfoldl body
  -- so Data Sub should NOT be unused
  liftIO $ assertBoolVerbose
    ("Expected Data Sub NOT to be unused but got: " ++ show unusedPairs
      ++ "\nDeps: " ++ show deps)
    (("Data", "Sub") `notElem` unusedPairs)

-- | An empty instance whose default method needs Generic (mirrors Binary+Generic).
-- Using the outer instance should keep Generic alive.
test_findUnusedInstances_defaultMethodGenericDep :: Assertion
test_findUnusedInstances_defaultMethodGenericDep = withPluginPopulatedDB
  [ ("Y.hs", unlines
      [ "{-# LANGUAGE DefaultSignatures, DeriveGeneric #-}"
      , "module Y where"
      , "import GHC.Generics (Generic)"
      , "class MyBinary a where"
      , "  myPut :: a -> String"
      , "  default myPut :: (Generic a, Show a) => a -> String"
      , "  myPut = show"
      , "data Cfg = Cfg { val :: Int } deriving (Show, Generic)"
      , "instance MyBinary Cfg"
      , "test :: String"
      , "test = myPut (Cfg 1)"
      ])
  ] $ \_tmpDir -> do
  unused <- getUnusedInstances
  deps <- getInstanceDeps
  instances <- getAllInstances
  let unusedPairs = map (\(_, _, cls, typ, _, _, _, _, _) -> (cls, typ)) unused
  -- MyBinary Cfg is used; its default myPut needs Generic Cfg
  -- so Generic Cfg should NOT be unused
  liftIO $ assertBoolVerbose
    ("Expected Generic Cfg NOT to be unused but got: " ++ show unusedPairs
      ++ "\nDeps: " ++ show deps
      ++ "\nInstances: " ++ show instances)
    (not $ any (\(c, _) -> c == "Generic") unusedPairs)

-- | NFData with a list field: NFData Outer depends on NFData [Inner]
-- which depends on NFData Inner.  NFData Inner should be kept alive.
-- This mirrors the NFData SchemaDefaultPermissionMetadata case.
test_findUnusedInstances_nfdataListFieldDep :: Assertion
test_findUnusedInstances_nfdataListFieldDep = withPluginPopulatedDB
  [ ("NF.hs", unlines
      [ "module NF where"
      , "import Control.DeepSeq (NFData(..))"
      , "data Inner = Inner Int"
      , "instance NFData Inner where rnf (Inner x) = rnf x"
      , "data Outer = Outer [Inner]"
      , "instance NFData Outer where rnf (Outer xs) = rnf xs"
      , "test :: Outer -> ()"
      , "test x = rnf x"
      ])
  ] $ \_tmpDir -> do
  unused <- getUnusedInstances
  deps <- getInstanceDeps
  let unusedPairs = map (\(_, _, cls, typ, _, _, _, _, _) -> (cls, typ)) unused
  -- NFData Outer is used; its rnf calls rnf on [Inner] which needs NFData Inner
  liftIO $ assertBoolVerbose
    ("Expected NFData Inner NOT to be unused but got: " ++ show unusedPairs
      ++ "\nDeps: " ++ show deps)
    (("NFData", "Inner") `notElem` unusedPairs)

-- | Derived NFData with list field — matches the bug report NFData pattern.
-- GHC DeriveAnyClass creates $crnf that references $fNFDataInner via
-- evidence in method body expressions.
test_findUnusedInstances_derivedNfdataListDep :: Assertion
test_findUnusedInstances_derivedNfdataListDep = withPluginPopulatedDB
  [ ("NF2.hs", unlines
      [ "{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}"
      , "module NF2 where"
      , "import GHC.Generics (Generic)"
      , "import Control.DeepSeq (NFData)"
      , "data Inner = Inner Int deriving (Generic, NFData)"
      , "data Outer = Outer [Inner] deriving (Generic, NFData)"
      , "-- no usage here to test dep extraction only"
      ])
  ] $ \_tmpDir -> do
  deps <- getInstanceDeps
  let depPairs = map (\(_, cls, typ) -> (cls, typ)) deps
  -- NFData Outer should depend on NFData Inner
  liftIO $ assertBoolVerbose
    ("Expected dep (NFData, Inner) in instance_deps but got: " ++ show deps)
    (("NFData", "Inner") `elem` depPairs)

-- | Cross-module transitive dependency: module A defines a type with derived
-- instances, module B imports it, wraps it in a new type that also derives
-- the same class, and uses the outer type.  The inner type's instance should
-- be marked as used transitively.
test_findUnusedInstances_crossModuleDep :: Assertion
test_findUnusedInstances_crossModuleDep = withPluginPopulatedDB
  [ ("CM/Inner.hs", unlines
      [ "module CM.Inner where"
      , "data Sub = Sub Int deriving (Show, Eq)"
      ])
  , ("CM/Outer.hs", unlines
      [ "module CM.Outer where"
      , "import CM.Inner (Sub(..))"
      , "data Container = Container Sub deriving (Show)"
      , "test :: String"
      , "test = show (Container (Sub 1))"
      ])
  ] $ \_tmpDir -> do
  unused <- getUnusedInstances
  deps <- getInstanceDeps
  instances <- getAllInstances
  let unusedPairs = map (\(_, _, cls, typ, _, _, _, _, _) -> (cls, typ)) unused
  -- Show Container is used (via show); it depends on Show Sub (cross-module)
  -- so Show Sub should NOT be unused.  Eq Sub IS unused.
  liftIO $ assertBoolVerbose
    ("Expected Show Sub NOT to be unused (cross-module dep) but got: " ++ show unusedPairs
      ++ "\nDeps: " ++ show deps
      ++ "\nInstances: " ++ show instances)
    (("Show", "Sub") `notElem` unusedPairs)
  liftIO $ assertBoolVerbose
    ("Expected Eq Sub to be unused but got: " ++ show unusedPairs)
    (("Eq", "Sub") `elem` unusedPairs)

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
