{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE OverloadedStrings #-}

module PluginTests ( htf_thisModulesTests ) where

import GHC.Stack
import Test.Framework
import Test.Framework.HUnitWrapper
import Test.HUnit.Base (Assertion)

import Data.List

import GHC
import GHC.Paths ( libdir )
import DynFlags
import HscTypes
import Module
import Plugins

import Language.Haskell.HsTools.Utils
import Language.Haskell.HsTools.Database

import Language.Haskell.HsTools.Plugin ()

import Debug.Trace

test_empty :: Assertion
test_empty = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where"] (runGhcTest conn)
  names <- getAllNames conn
  assertEqual [] names

test_oneDef :: Assertion
test_oneDef = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "x = ()"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 1, Global "X.x", "()", Definition)
  gsubAssert $ assertHasName names (2, 5, Global "GHC.Tuple.()", "()", Use)

test_localDef :: Assertion
test_localDef = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "x = y where y = ()"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 1, Global "X.x", "()", Definition)
  gsubAssert $ assertHasName names (2, 5, Local "X.y", "()", Use)
  gsubAssert $ assertHasName names (2, 13, Local "X.y", "()", Definition)
  gsubAssert $ assertHasName names (2, 17, Global "GHC.Tuple.()", "()", Use)

test_oneDefWithTypeSig :: Assertion
test_oneDefWithTypeSig = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "x :: ()", "x = ()"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 1, Global "X.x", "()", Definition)
  -- gsubAssert $ assertHasName names (2, 6, "()", Just "*", Use)
  gsubAssert $ assertHasName names (3, 1, Global "X.x", "()", Definition)
  gsubAssert $ assertHasName names (3, 5, Global "GHC.Tuple.()", "()", Use)

test_dataDef :: Assertion
test_dataDef = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "data X = Y ()"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 6, Global "X.X", "*", Definition)
  gsubAssert $ assertHasName names (2, 10, Global "X.Y", "() -> X", Definition)
  -- gsubAssert $ assertHasName names (2, 12, Global "()", "*", Use)



test_reStore :: Assertion
test_reStore = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where"] (runGhcTest conn)
  withTestFileLines testFile ["module X where", "x = ()"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 1, Global "X.x", "()", Definition)
  gsubAssert $ assertHasName names (2, 5, Global "GHC.Tuple.()", "()", Use)

------------------------------------------------------------------------------

data NameDefinition = Global String | Local String
data NameRole = Definition | Use
  deriving (Eq)

assertHasName :: HasCallStack => [(Int, Int, String, Maybe String, Bool)] -> (Int, Int, NameDefinition, String, NameRole) -> Assertion
assertHasName names expected = assertBoolVerbose ("actual names: " ++ show names) $ any (matchRow expected) names
  where matchRow (l, c, nd, t, nr) (l', c', n, t', d) =
          l == l' && c == c' && (nr == Definition) == d && Just t == t'
            && (case nd of Global n' -> n == n'; Local n' -> (n' ++ ":") `isPrefixOf` n)

runGhcTest conn = do
  res <- defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- dynFlagsForTest
      setSessionDynFlags dflags
      target <- guessTarget testFile Nothing
      setTargets [target]
      load LoadAllTargets
  errors <- getErrors conn
  assertEqual [] errors
  assertEqual True (succeeded res)

testFile = "x.hs"

dynFlagsForTest = do
  flags <- getSessionDynFlags
  return flags
    { ghcLink = NoLink
    , hscTarget = HscNothing
    , pluginModNames = [pluginMod]
    , pluginModNameOpts = [(pluginMod, connectionString)]
    -- , pluginModNameOpts = [(pluginMod, "verbose"), (pluginMod, connectionString)]
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