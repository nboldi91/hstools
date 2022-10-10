{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE OverloadedStrings #-}

module PluginTests ( htf_thisModulesTests ) where

import Test.Framework
import Test.HUnit.Base (Assertion)

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
  assertEqual 
    [ (2, 1, "X.x", Just "()", True)
    , (2, 5, "GHC.Tuple.()", Just "()", False)
    ] names

-- test_oneDefWithTypeSig :: Assertion
-- test_oneDefWithTypeSig = useTestRepo $ \conn -> do
--   withTestFileLines testFile ["module X where", "x :: ()", "x = ()"] (runGhcTest conn)
--   names <- getAllNames conn
--   assertEqual 
--     [ (2, 1, "X.x", Just "GHC.Tuple.()", True)
--     -- , (2, 6, "GHC.Tuple.()", Just "*", False) -- TODO
--     , (3, 1, "X.x", Just "GHC.Tuple.()", True)
--     , (3, 5, "GHC.Tuple.()", Just "()", False)
--     ] names

-- test_dataDef :: Assertion
-- test_dataDef = useTestRepo $ \conn -> do
--   withTestFileLines testFile ["module X where", "data X = Y String"] runGhcTest
--   names <- getAllNames conn
--   assertEqual 
--     [ (2, 6, "X.X", Just "*", True)
--     , (2, 10, "X.Y", Just "String -> X", True)
--     , (2, 12, "String", Just "*", False)
--     ] names

------------------------------------------------------------------------------

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
    -- , pluginModNameOpts = [(pluginMod, connectionString)]
    , pluginModNameOpts = [(pluginMod, "verbose"), (pluginMod, connectionString)]
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