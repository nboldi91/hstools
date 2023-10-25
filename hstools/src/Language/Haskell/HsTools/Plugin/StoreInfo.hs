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
import Database.PostgreSQL.Simple (Connection)

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

import Language.Haskell.HsTools.Plugin.Utils.DebugGhcAST ()

storeParsed :: StoreParams -> HsParsedModule -> IO ()
storeParsed (StoreParams isVerbose conn (moduleName, moduleId)) md = do
    let modEnd = listToMaybe =<< Map.lookup (noSrcSpan, AnnEofPos) (fst $ hpm_annotations md)
        modSpan = maybe id combineSrcSpans modEnd $ getLoc $ hpm_module md
        storeModule = storeLoc store (L modSpan (unLoc $ hpm_module md))
    context <- defaultStoreContext conn moduleId moduleName (Just modSpan)
    ((), defs) <- runWriterT (runReaderT storeModule context)
    let sortedDefs = nub $ sort defs
    when isVerbose $ do
      putStrLn "### Storing ast definitions:"
      mapM_ print sortedDefs
    astIds <- persistAst conn (map convertLocation sortedDefs)
    defIds <- persistDefinitions conn (catMaybes $ map convertDefinition $ sortedDefs `zip` astIds)
    persistComments conn $ findDefinitionOfComments moduleId (snd $ hpm_annotations md) (sortedDefs `zip` defIds)
    persistName conn (catMaybes $ map convertName (sortedDefs `zip` astIds))
  where
    convertLocation p = (moduleId, startRow, startColumn, endRow, endColumn)
      where NodePos startRow startColumn endRow endColumn = prPos p
    convertDefinition ((ParseDefinitionRecord kind _), astId) = Just (moduleId, astId, kind)
    convertDefinition _ = Nothing
    convertName ((ParseModuleName mn _ isDefined definition), astId) =
      Just (moduleId, astId :: Int, mn, Just $ fromEnum ModuleNS, isDefined, fmap npStartRow definition, fmap npStartCol definition, fmap npEndRow definition, fmap npEndCol definition)
    convertName _ = Nothing

storeNames :: StoreParams -> ([Located (IE GhcRn)], [LImportDecl GhcRn], HsGroup GhcRn) -> IO ()
storeNames (StoreParams isVerbose conn (moduleName, moduleId)) gr = do
  context <- defaultStoreContext conn moduleId moduleName Nothing
  ((), names) <- runWriterT (runReaderT (store gr) context)
  let uniqueNames = nub $ sort names
  when isVerbose $ do
    putStrLn "### Storing names:"
    mapM_ print uniqueNames
  persistNames conn moduleId uniqueNames

storeMain :: StoreParams -> Maybe Name -> IO ()
storeMain (StoreParams isVerbose conn (moduleName, moduleId)) (Just nm) = do
  when isVerbose $ do
    putStrLn $ "### Storing main: " ++ show nm
  persistMain conn moduleId (generateFullName moduleName nm)
storeMain _ Nothing = return ()

persistNames :: Connection -> Int -> [NameRecord] -> IO ()
persistNames conn moduleId names = do
    astIds <- persistAst conn (map convertLocation names)
    persistName conn (map convertName (names `zip` astIds))
  where
    convertName ((NameRecord name namespace isDefined definition _), id) =
      (moduleId, id :: Int, name, fmap fromEnum namespace, isDefined, fmap npStartRow definition, fmap npStartCol definition, fmap npEndRow definition, fmap npEndCol definition)
    convertLocation (NameRecord { nmPos = NodePos startRow startColumn endRow endColumn }) =
      (moduleId, startRow, startColumn, endRow, endColumn)

storeTypes :: StoreParams -> TcGblEnv -> IO ()
storeTypes (StoreParams isVerbose conn (moduleName, moduleId)) env = do
  context <- defaultStoreContext conn moduleId moduleName Nothing
  -- putStrLn $ show $ tcg_binds env
  let storeEnv = do
        store $ tcg_binds env
        store $ eltsUFM $ tcg_type_env env
  ((), types) <- runWriterT (runReaderT storeEnv context)
  let uniqueTypes = nub $ sort types
  when isVerbose $ do
    putStrLn "### Storing types:"
    mapM_ print uniqueTypes
  persistTypes conn (map convertType uniqueTypes)
  where
    convertType (TypeRecord name namespace typ) = (name, fmap fromEnum namespace, typ)

storeTHNamesAndTypes :: StoreParams -> LHsExpr GhcTc -> IO ()
storeTHNamesAndTypes (StoreParams isVerbose conn (moduleName, moduleId)) expr = do
    context <- defaultStoreContext conn moduleId moduleName Nothing
    ((), namesAndTypes) <- runWriterT (runReaderT (store expr) context)
    when isVerbose $ do
      putStrLn "### Storing names and types for TH:"
      mapM_ print namesAndTypes
    persistNamesAndTypes conn moduleId namesAndTypes
    persistTHRange conn (getLoc expr) moduleId namesAndTypes

persistNamesAndTypes :: Connection -> Int -> [NameAndTypeRecord] -> IO ()
persistNamesAndTypes conn moduleId namesAndTypes = do
    astIds <- persistAst conn (map convertLocation namesAndTypes)
    persistName conn (map convertName (namesAndTypes `zip` astIds))
    persistTypes conn (catMaybes $ map convertType namesAndTypes)
  where
    convertName (NameAndTypeRecord { ntrName, ntrNamespace, ntrIsDefined }, id) = (moduleId, id :: Int, ntrName, fmap fromEnum ntrNamespace, ntrIsDefined, Nothing, Nothing, Nothing, Nothing)
    convertType (NameAndTypeRecord { ntrName, ntrNamespace, ntrType }) = fmap (ntrName, fmap fromEnum ntrNamespace, ) ntrType
    convertLocation (NameAndTypeRecord { ntrPos = NodePos startRow startColumn endRow endColumn })
      = (moduleId, startRow, startColumn, endRow, endColumn)

persistTHRange :: Connection -> SrcSpan -> Int -> [NameAndTypeRecord] -> IO ()
persistTHRange conn (RealSrcSpan sp) moduleId records = do
  let nodePos = realSrcSpanToNodePos sp
  astNode <- if nodePos `elem` (map ntrPos records)
    then getAstId conn moduleId (npStartRow nodePos) (npStartCol nodePos) (npEndRow nodePos) (npEndCol nodePos)
    else insertAstId conn moduleId (npStartRow nodePos) (npStartCol nodePos) (npEndRow nodePos) (npEndCol nodePos)
  persistTHRange' conn moduleId astNode
persistTHRange _ _ _ _ = return () 
