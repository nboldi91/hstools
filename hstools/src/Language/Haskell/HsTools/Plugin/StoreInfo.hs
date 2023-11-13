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
import qualified Data.Set as Set
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
import Language.Haskell.HsTools.Utils (LogOptions(..))

import Language.Haskell.HsTools.Plugin.Utils.DebugGhcAST ()

storeParsed :: HsParsedModule -> StoreStageM ()
storeParsed md = do
    StoreParams logOptions _ (_, moduleId) <- ask
    let modEnd = listToMaybe =<< Map.lookup (noSrcSpan, AnnEofPos) (fst $ hpm_annotations md)
        modSpan = maybe id combineSrcSpans modEnd $ getLoc $ hpm_module md
        storeModule = storeLoc store (L modSpan (unLoc $ hpm_module md))
    context <- defaultStoreContext (Just modSpan)
    ((), defs) <- liftIO $ runWriterT $ runReaderT storeModule context
    let sortedDefs = nub $ sort defs
    when (logOptionsFullData logOptions) $ liftIO $ do
      putStrLn "Storing ast definitions:"
      mapM_ print sortedDefs
    let
      convertLocation p = FullRange moduleId (prPos p)
      convertDefinition ((ParseDefinitionRecord kind _), astId) = Just (moduleId, astId, kind)
      convertDefinition _ = Nothing
      convertName ((ParseModuleName mn _ isDefined definition), astId) =
        Just (moduleId, astId :: Int, FullName mn Nothing (Just ModuleNS), isDefined, definition)
      convertName _ = Nothing
    withReaderT storeParamsDbConn $ do
      astIds <- persistAst (map convertLocation sortedDefs)
      defIds <- persistDefinitions (catMaybes $ map convertDefinition $ sortedDefs `zip` astIds)
      persistComments $ findDefinitionOfComments moduleId (snd $ hpm_annotations md) (sortedDefs `zip` defIds)
      persistName (catMaybes $ map convertName (sortedDefs `zip` astIds))

storeNames :: ([Located (IE GhcRn)], [LImportDecl GhcRn], HsGroup GhcRn) -> StoreStageM ()
storeNames gr = do
  StoreParams logOptions _ (_, moduleId) <- ask
  context <- defaultStoreContext Nothing
  ((), names) <- liftIO $ runWriterT $ runReaderT (store gr) context
  let uniqueNames = Set.toList $ Set.fromList names
  when (logOptionsFullData logOptions) $ liftIO $ do
    putStrLn "Storing names:"
    mapM_ print uniqueNames
  withReaderT storeParamsDbConn $
    persistNames moduleId uniqueNames

storeMain :: Maybe Name -> StoreStageM ()
storeMain (Just nm) = do
  StoreParams logOptions _ (moduleName, moduleId) <- ask
  when (logOptionsFullData logOptions) $ liftIO $ do
    putStrLn $ "Storing main: " ++ show nm
  withReaderT storeParamsDbConn $
    persistMain moduleId (fnName $ generateFullName moduleName nm)
storeMain Nothing = return ()

persistNames :: Int -> [NameRecord] -> PersistStageM ()
persistNames moduleId names = do
    astIds <- persistAst (map (FullRange moduleId . nmPos) names)
    persistName (map convertName (names `zip` astIds))
  where
    convertName ((NameRecord name isDefined definition _), id) =
      (moduleId, id :: Int, name, isDefined, definition)

storeTypes :: TcGblEnv -> StoreStageM ()
storeTypes env = do
  StoreParams logOptions _ _ <- ask
  context <- defaultStoreContext Nothing
  let storeEnv = do
        store $ tcg_binds env
        store $ eltsUFM $ tcg_type_env env
  ((), types) <- liftIO $ runWriterT $ runReaderT storeEnv context
  let uniqueTypes = Set.toList $ Set.fromList types
  when (logOptionsFullData logOptions) $ liftIO $ do
    putStrLn "Storing types:"
    mapM_ print uniqueTypes
  withReaderT storeParamsDbConn $
    persistTypes (map convertType uniqueTypes)
  where
    convertType (TypeRecord name typ) = (name, typ)

storeTHNamesAndTypes :: LHsExpr GhcTc -> StoreStageM ()
storeTHNamesAndTypes expr = do
    StoreParams logOptions _ (_, moduleId) <- ask
    context <- defaultStoreContext Nothing
    ((), namesAndTypes) <- liftIO $ runWriterT (runReaderT (store expr) context)
    when (logOptionsFullData logOptions) $ liftIO $ do
      putStrLn "Storing names and types for TH:"
      mapM_ print namesAndTypes
    withReaderT storeParamsDbConn $ do
      persistNamesAndTypes moduleId namesAndTypes
      persistTHRange (getLoc expr) moduleId namesAndTypes

persistNamesAndTypes :: Int -> [NameAndTypeRecord] -> PersistStageM ()
persistNamesAndTypes moduleId namesAndTypes = do
    astIds <- persistAst (map (FullRange moduleId . ntrPos) namesAndTypes)
    persistName (map convertName (namesAndTypes `zip` astIds))
    persistTypes (catMaybes $ map convertType namesAndTypes)
  where
    convertName (NameAndTypeRecord { ntrName, ntrIsDefined }, id) = (moduleId, id :: Int, ntrName, ntrIsDefined, Nothing)
    convertType (NameAndTypeRecord { ntrName, ntrType }) = fmap (ntrName, ) ntrType

persistTHRange :: SrcSpan -> Int -> [NameAndTypeRecord] -> PersistStageM ()
persistTHRange (RealSrcSpan sp) moduleId records = do
  let nodePos = realSrcSpanToNodePos sp
      fullRange = FullRange moduleId nodePos
  astNode <- if nodePos `elem` (map ntrPos records)
    then getAstId fullRange
    else insertAstId fullRange
  persistTHRange' moduleId astNode
persistTHRange _ _ _ = return () 
