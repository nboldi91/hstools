{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.HsTools.Plugin.StoreInfo where

import Control.Monad.Writer
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Map as Map
import Data.List

import ApiAnnotation
import HsDecls
import HscTypes
import HsExpr
import HsExtension
import HsImpExp
import Name
import SrcLoc
import TcRnTypes
import UniqFM

import Language.Haskell.HsTools.Plugin.Monad
import Language.Haskell.HsTools.Plugin.Types
import Language.Haskell.HsTools.Plugin.Storable
import Language.Haskell.HsTools.Plugin.StorableInstances (generateFullName)
import Language.Haskell.HsTools.Plugin.StoreComments
import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.Utils (DbConn, Verbosity(..))

import Language.Haskell.HsTools.Plugin.Utils.DebugGhcAST ()

storeParsed :: StoreParams -> HsParsedModule -> IO ()
storeParsed sp@(StoreParams verbosity _ (moduleName, moduleId)) md = do
    let conn = storeParamsDbConn sp
        modEnd = listToMaybe =<< Map.lookup (noSrcSpan, AnnEofPos) (fst $ hpm_annotations md)
        modSpan = maybe id combineSrcSpans modEnd $ getLoc $ hpm_module md
        storeModule = storeLoc store (L modSpan (unLoc $ hpm_module md))
    context <- defaultStoreContext conn moduleId moduleName (Just modSpan)
    ((), defs) <- runWriterT (runReaderT storeModule context)
    let sortedDefs = nub $ sort defs
    when (verbosity >= VerbosityVerbose) $ do
      putStrLn "### Storing ast definitions:"
      mapM_ print sortedDefs
    astIds <- persistAst conn (map convertLocation sortedDefs)
    defIds <- persistDefinitions conn (catMaybes $ map convertDefinition $ sortedDefs `zip` astIds)
    persistComments conn $ findDefinitionOfComments moduleId (snd $ hpm_annotations md) (sortedDefs `zip` defIds)
    persistName conn (catMaybes $ map convertName (sortedDefs `zip` astIds))
  where
    convertLocation p = FullRange moduleId (prPos p)
    convertDefinition ((ParseDefinitionRecord kind _), astId) = Just (moduleId, astId, kind)
    convertDefinition _ = Nothing
    convertName ((ParseModuleName mn _ isDefined definition), astId) =
      Just (moduleId, astId :: Int, FullName mn Nothing (Just ModuleNS), isDefined, definition)
    convertName _ = Nothing

storeNames :: StoreParams -> ([Located (IE GhcRn)], [LImportDecl GhcRn], HsGroup GhcRn) -> IO ()
storeNames sp@(StoreParams verbosity _ (moduleName, moduleId)) gr = do
  let conn = storeParamsDbConn sp
  context <- defaultStoreContext conn moduleId moduleName Nothing
  ((), names) <- runWriterT (runReaderT (store gr) context)
  let uniqueNames = nub $ sort names
  when (verbosity >= VerbosityVerbose) $ do
    putStrLn "### Storing names:"
    mapM_ print uniqueNames
  persistNames conn moduleId uniqueNames

storeMain :: StoreParams -> Maybe Name -> IO ()
storeMain sp@(StoreParams verbosity _ (moduleName, moduleId)) (Just nm) = do
  let conn = storeParamsDbConn sp
  when (verbosity >= VerbosityVerbose) $ do
    putStrLn $ "### Storing main: " ++ show nm
  persistMain conn moduleId (fnName $ generateFullName moduleName nm)
storeMain _ Nothing = return ()

persistNames :: DbConn -> Int -> [NameRecord] -> IO ()
persistNames conn moduleId names = do
    astIds <- persistAst conn (map (FullRange moduleId . nmPos) names)
    persistName conn (map convertName (names `zip` astIds))
  where
    convertName ((NameRecord name isDefined definition _), id) =
      (moduleId, id :: Int, name, isDefined, definition)

storeTypes :: StoreParams -> TcGblEnv -> IO ()
storeTypes sp@(StoreParams verbosity _ (moduleName, moduleId)) env = do
  let conn = storeParamsDbConn sp
  context <- defaultStoreContext conn moduleId moduleName Nothing
  -- putStrLn $ show $ tcg_binds env
  let storeEnv = do
        store $ tcg_binds env
        store $ eltsUFM $ tcg_type_env env
  ((), types) <- runWriterT (runReaderT storeEnv context)
  let uniqueTypes = nub $ sort types
  when (verbosity >= VerbosityVerbose) $ do
    putStrLn "### Storing types:"
    mapM_ print uniqueTypes
  persistTypes conn (map convertType uniqueTypes)
  where
    convertType (TypeRecord name typ) = (name, typ)

storeTHNamesAndTypes :: StoreParams -> LHsExpr GhcTc -> IO ()
storeTHNamesAndTypes sp@(StoreParams verbosity _ (moduleName, moduleId)) expr = do
    let conn = storeParamsDbConn sp
    context <- defaultStoreContext conn moduleId moduleName Nothing
    ((), namesAndTypes) <- runWriterT (runReaderT (store expr) context)
    when (verbosity >= VerbosityVerbose) $ do
      putStrLn "### Storing names and types for TH:"
      mapM_ print namesAndTypes
    persistNamesAndTypes conn moduleId namesAndTypes
    persistTHRange conn (getLoc expr) moduleId namesAndTypes

persistNamesAndTypes :: DbConn -> Int -> [NameAndTypeRecord] -> IO ()
persistNamesAndTypes conn moduleId namesAndTypes = do
    astIds <- persistAst conn (map (FullRange moduleId . ntrPos) namesAndTypes)
    persistName conn (map convertName (namesAndTypes `zip` astIds))
    persistTypes conn (catMaybes $ map convertType namesAndTypes)
  where
    convertName (NameAndTypeRecord { ntrName, ntrIsDefined }, id) = (moduleId, id :: Int, ntrName, ntrIsDefined, Nothing)
    convertType (NameAndTypeRecord { ntrName, ntrType }) = fmap (ntrName, ) ntrType

persistTHRange :: DbConn -> SrcSpan -> Int -> [NameAndTypeRecord] -> IO ()
persistTHRange conn (RealSrcSpan sp) moduleId records = do
  let nodePos = realSrcSpanToNodePos sp
      fullRange = FullRange moduleId nodePos
  astNode <- if nodePos `elem` (map ntrPos records)
    then getAstId conn fullRange
    else insertAstId conn fullRange
  persistTHRange' conn moduleId astNode
persistTHRange _ _ _ _ = return () 
