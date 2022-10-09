{-# OPTIONS_GHC -F -pgmF htfpp #-}
module PluginTests ( htf_thisModulesTests ) where

import Test.Framework
import Test.HUnit.Base (Assertion)

import GHC
import GHC.Paths ( libdir )
import DynFlags

import Language.Haskell.HsTools.Utils
import Language.Haskell.HsTools.Database

import Language.Haskell.HsTools.Plugin ()

test_empty :: Assertion
test_empty = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where"] runGhcTest
  names <- getAllNames conn
  assertEqual [] names

test_oneDef :: Assertion
test_oneDef = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "x = ()"] runGhcTest
  names <- getAllNames conn
  assertEqual [(2, 1, "X.x", True), (2, 5, "GHC.Tuple.()", False)] names

------------------------------------------------------------------------------

runGhcTest = do
  res <- defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- dynFlagsForTest
      setSessionDynFlags dflags
      target <- guessTarget testFile Nothing
      setTargets [target]
      load LoadAllTargets
  assertEqual True (succeeded res)

testFile = "x.hs"

dynFlagsForTest = do
  flags <- getSessionDynFlags
  return flags
    { ghcLink = NoLink
    , hscTarget = HscNothing
    , pluginModNames = [pluginMod]
    , pluginModNameOpts = [(pluginMod, connectionString)]
    }

useTestRepo = withTestRepo connectionStringWithoutDB connectionDBName

connectionStringWithoutDB :: String
connectionStringWithoutDB = "postgresql://saver:saver@127.0.0.1:5432"

connectionString :: String
connectionString = connectionStringWithoutDB ++ "/" ++ connectionDBName

connectionDBName :: String
connectionDBName = "plugintestrepo"

pluginMod :: ModuleName
pluginMod = mkModuleName "Language.Haskell.HsTools.Plugin"