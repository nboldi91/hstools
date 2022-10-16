{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PluginTests ( htf_thisModulesTests ) where

import GHC.Stack
import Test.Framework
import Test.Framework.HUnitWrapper
import Test.HUnit.Base (Assertion)

import Data.List
import Control.Exception (SomeException)
import Control.Monad.IO.Class

import GHC
import GHC.Paths ( libdir )
import qualified EnumSet as ES
import Exception
import DynFlags
import HscTypes
import Module
import Plugins

import Language.Haskell.HsTools.Utils
import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.HandleErrors

import Language.Haskell.HsTools.Plugin ()

------------------------------------------
--- language test cases

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

test_polyDef :: Assertion
test_polyDef = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "x :: Num a => a -> a -> a", "x = (+)"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 1, Global "X.x", "forall a. Num a => a -> a -> a", Definition)
  gsubAssert $ assertHasNameNoType names (2, 6, Global "GHC.Num.Num", Use) -- TODO: kind should be "* -> * -> Constraint"
  gsubAssert $ assertHasNameNoType names (2, 10, Local "X.a", Use) -- TODO: kind should be "* -> * -> Constraint"
  gsubAssert $ assertHasNameNoType names (2, 15, Local "X.a", Use) -- TODO: kind should be "* -> * -> Constraint"
  gsubAssert $ assertHasName names (3, 1, Global "X.x", "forall a. Num a => a -> a -> a", Definition)
  gsubAssert $ assertHasName names (3, 5, Global "GHC.Num.+", "forall a. Num a => a -> a -> a", Use)

test_localDef :: Assertion
test_localDef = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "x = y where y = ()"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 1, Global "X.x", "()", Definition)
  gsubAssert $ assertHasName names (2, 5, Local "X.y", "()", Use)
  gsubAssert $ assertHasName names (2, 13, Local "X.y", "()", Definition)
  gsubAssert $ assertHasName names (2, 17, Global "GHC.Tuple.()", "()", Use)

test_localLetDef :: Assertion
test_localLetDef = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "x = let y = 3 in y"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 1, Global "X.x", "Integer", Definition)
  gsubAssert $ assertHasName names (2, 9, Local "X.y", "Integer", Definition)
  gsubAssert $ assertHasName names (2, 18, Local "X.y", "Integer", Use)

test_oneDefWithTypeSig :: Assertion
test_oneDefWithTypeSig = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "x :: ()", "x = ()"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 1, Global "X.x", "()", Definition)
  gsubAssert $ assertHasNameNoType names (2, 6, Global "GHC.Tuple.()", Use) -- TODO: kind should be *
  gsubAssert $ assertHasName names (3, 1, Global "X.x", "()", Definition)
  gsubAssert $ assertHasName names (3, 5, Global "GHC.Tuple.()", "()", Use)

test_dataDef :: Assertion
test_dataDef = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "data X = Y { y :: () }"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 6, Global "X.X", "*", Definition)
  gsubAssert $ assertHasName names (2, 10, Global "X.Y", "() -> X", Definition)
  gsubAssert $ assertHasName names (2, 14, Global "X.y", "X -> ()", Definition)
  gsubAssert $ assertHasNameNoType names (2, 19, Global "GHC.Tuple.()", Use) -- TODO: kind should be *

test_newTypeDef :: Assertion
test_newTypeDef = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "newtype X a = X a"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 9, Global "X.X", "* -> *", Definition)
  gsubAssert $ assertHasNameNoType names (2, 11, Local "X.a", Definition) -- TODO: kind should be *
  gsubAssert $ assertHasName names (2, 15, Global "X.X", "forall a. a -> X a", Definition)
  gsubAssert $ assertHasNameNoType names (2, 17, Local "X.a", Use) -- TODO: kind should be *

test_classDef :: Assertion
test_classDef = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "class C a where c :: a -> a"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 7, Global "X.C", "* -> Constraint", Definition)
  gsubAssert $ assertHasNameNoType names (2, 9, Local "X.a", Definition) -- TODO: kind should be *
  gsubAssert $ assertHasName names (2, 17, Global "X.c", "forall a. C a => a -> a", Definition)
  gsubAssert $ assertHasNameNoType names (2, 22, Local "X.a", Use) -- TODO: kind should be *
  gsubAssert $ assertHasNameNoType names (2, 27, Local "X.a", Use) -- TODO: kind should be *

test_instanceDef :: Assertion
test_instanceDef = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "data X = X", "instance Show X where", "  show X = \"x\""] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasNameNoType names (3, 10, Global "GHC.Show.Show", Use) -- TODO: kind should be "* -> Constraint"
  gsubAssert $ assertHasName names (3, 15, Global "X.X", "*", Use)
  gsubAssert $ assertHasNameNoType names (4, 3, Global "GHC.Show.show", Use) -- TODO: kind should be "X -> String"
  gsubAssert $ assertHasName names (4, 8, Global "X.X", "X", Use)

test_typeDef :: Assertion
test_typeDef = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "type MyString = String"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 6, Global "X.MyString", "*", Definition)
  gsubAssert $ assertHasNameNoType names (2, 17, Global "GHC.Base.String", Use) -- TODO: kind should be *

test_fullDataType :: Assertion
test_fullDataType = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "data A a = B a a | C { x :: a }"] (runGhcTest conn)
  defs <- getAllDefinitions conn
  assertEqual
    [ (DefTypeDecl, Just "X.A", 2, 1, 2, 32)
    , (DefConstructor, Just "X.B", 2, 12, 2, 17)
    , (DefCtorArg, Nothing, 2, 14, 2, 15)
    , (DefCtorArg, Nothing, 2, 16, 2, 17)
    , (DefConstructor, Just "X.C", 2, 20, 2, 32)
    , (DefCtorArg, Just "X.x", 2, 24, 2, 25)
    ] defs

test_openTypeFamily :: Assertion
test_openTypeFamily = useTestRepo $ \conn -> do
  withTestFileLines testFile ["{-# LANGUAGE TypeFamilies #-}", "module X where", "type family Not a"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (3, 13, Global "X.Not", "* -> *", Definition)
  gsubAssert $ assertHasNameNoType names (3, 17, Local "X.a", Definition) -- TODO: kind should be *

test_closedTypeFamily :: Assertion
test_closedTypeFamily = useTestRepo $ \conn -> do
  withTestFileLines testFile ["{-# LANGUAGE TypeFamilies, DataKinds #-}", "module X where", "type family Not a where", "  Not True = False", "  Not False = True"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (3, 13, Global "X.Not", "Bool -> Bool", Definition)
  gsubAssert $ assertHasNameNoType names (3, 17, Local "X.a", Definition) -- TODO: kind should be *
  gsubAssert $ assertHasName names (4, 3, Global "X.Not", "Bool -> Bool", Definition)
  gsubAssert $ assertHasNameNoType names (4, 7, Global "GHC.Types.True", Use) -- TODO: type should be Bool

test_importedFunction :: Assertion
test_importedFunction = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "import Data.List", "x = intercalate \"->\" [\"a\",\"b\"]"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (3, 1, Global "X.x", "[Char]", Definition)
  gsubAssert $ assertHasName names (3, 5, Global "Data.OldList.intercalate", "forall a. [a] -> [[a]] -> [a]", Use)

-----------------------------------------
--- technical test cases

test_comments :: Assertion
test_comments = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "-- | comment for x", "x :: ()", "-- ^ another comment for x", "x = ()"] (runGhcTest conn)
  defs <- getAllDefinitions conn
  assertEqual
    [ (DefSignature, Just "X.x", 3, 1, 3, 8)
    , (DefValue, Just "X.x", 5, 1, 5, 7)
    ] defs
  comments <- getAllComments conn
  assertEqual [(3, 1, "-- ^ another comment for x"), (3, 1, "-- | comment for x")] comments

test_commentsInLineOnArgs :: Assertion
test_commentsInLineOnArgs = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "f :: String {-^ arg1 -} -> {-| arg2 -} Int -> String {-^ result -}", "f \"\" _ = \"\""] (runGhcTest conn)
  defs <- getAllDefinitions conn
  assertEqual
    [ (DefSignature, Just "X.f", 2, 1, 2, 53)
    , (DefParameter, Nothing, 2, 6, 2, 12)
    , (DefParameter, Nothing, 2, 40, 2, 43)
    , (DefParameter, Nothing, 2, 47, 2, 53)
    , (DefValue, Just "X.f", 3, 1, 3, 12)
    ] defs
  comments <- getAllComments conn
  assertEqual
    [ (2, 6, "{-^ arg1 -}")
    , (2, 40, "{-| arg2 -}")
    , (2, 47, "{-^ result -}")
    ] comments

test_multipleModules :: Assertion
test_multipleModules = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "import Y", "x = y"] $ withTestFileLines "Y.hs" ["module Y where", "y = ()"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (3, 1, Global "X.x", "()", Definition)
  gsubAssert $ assertHasName names (3, 5, Global "Y.y", "()", Use)
  gsubAssert $ assertHasName names (2, 1, Global "Y.y", "()", Definition)

test_reStore :: Assertion
test_reStore = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where"] (runGhcTest conn)
  withTestFileLines testFile ["module X where", "x = ()"] (runGhcTest conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 1, Global "X.x", "()", Definition)
  gsubAssert $ assertHasName names (2, 5, Global "GHC.Tuple.()", "()", Use)

test_typeError :: Assertion
test_typeError = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "x = 4 + \"hello\""] (runGhcTestError conn)
  names <- getAllNames conn
  gsubAssert $ assertHasNameNoType names (2, 1, Global "X.x", Definition)

test_parseErrorShouldNotClearExistingData :: Assertion
test_parseErrorShouldNotClearExistingData = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "x = ()"] (runGhcTest conn)
  withTestFileLines testFile ["module X where", "("] (runGhcTestError conn)
  names <- getAllNames conn
  gsubAssert $ assertHasName names (2, 1, Global "X.x", "()", Definition)

test_namingErrorShouldNotClearExistingData :: Assertion
test_namingErrorShouldNotClearExistingData = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "x = ()"] (runGhcTest conn)
  withTestFileLines testFile ["module X where", "x = y"] (runGhcTestError conn)
  names <- getAllNames conn
  gsubAssert $ assertHasNameNoType names (2, 1, Global "X.x", Definition)

test_typeErrorShouldNotClearExistingData :: Assertion
test_typeErrorShouldNotClearExistingData = useTestRepo $ \conn -> do
  withTestFileLines testFile ["module X where", "x = ()"] (runGhcTest conn)
  withTestFileLines testFile ["module X where", "x = 4 + \"hello\""] (runGhcTestError conn)
  names <- getAllNames conn
  gsubAssert $ assertHasNameNoType names (2, 1, Global "X.x", Definition)

------------------------------------------------------------------------------

data NameDefinition = Global String | Local String
data NameRole = Definition | Use
  deriving (Eq)

assertHasName :: HasCallStack => [(Int, Int, String, Maybe String, Bool)] -> (Int, Int, NameDefinition, String, NameRole) -> Assertion
assertHasName names expected = assertBoolVerbose ("actual names: " ++ show names) $ any (matchRow expected) names
  where matchRow (l, c, nd, t, nr) (l', c', n, t', d) =
          l == l' && c == c' && (nr == Definition) == d && Just t == t'
            && (case nd of Global n' -> n == n'; Local n' -> (n' ++ ":") `isPrefixOf` n)

assertHasNameNoType :: HasCallStack => [(Int, Int, String, Maybe String, Bool)] -> (Int, Int, NameDefinition, NameRole) -> Assertion
assertHasNameNoType names expected = assertBoolVerbose ("actual names: " ++ show names) $ any (matchRow expected) names
  where matchRow (l, c, nd, nr) (l', c', n, t', d) =
          l == l' && c == c' && (nr == Definition) == d && Nothing == t'
            && (case nd of Global n' -> n == n'; Local n' -> (n' ++ ":") `isPrefixOf` n)

runGhcTestNoSuccessCheck conn = do
  res <- defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- dynFlagsForTest
      setSessionDynFlags dflags
      target <- guessTarget testFile Nothing
      setTargets [target]
      load LoadAllTargets
  errors <- getErrors conn
  assertEqual [] errors
  return res

runGhcTest conn = do
  res <- runGhcTestNoSuccessCheck conn
  assertEqual True (succeeded res)

runGhcTestError conn = do
  res <- runGhcTestNoSuccessCheck conn
  assertEqual True (failed res)

testFile = "X.hs"

dynFlagsForTest = do
  flags <- getSessionDynFlags
  return flags
    { ghcLink = NoLink
    , hscTarget = HscNothing
    , pluginModNames = [pluginMod]
    , generalFlags = ES.insert Opt_KeepRawTokenStream (generalFlags flags) -- use flag -dkeep-comments
    , pluginModNameOpts = [(pluginMod, connectionString)]
    -- , pluginModNameOpts = [(pluginMod, "verbose"), (pluginMod, connectionString)]
    , maxErrors = Just 0
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