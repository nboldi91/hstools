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
import Data.List
import Database.PostgreSQL.Simple (Connection)

import HsDecls
import HsExpr
import HsExtension
import HsSyn
import SrcLoc
import TcRnTypes
import UniqFM

import Language.Haskell.HsTools.Plugin.Monad
import Language.Haskell.HsTools.Plugin.Types
import Language.Haskell.HsTools.Plugin.Class
import Language.Haskell.HsTools.Database

import Language.Haskell.HsTools.Plugin.Utils.DebugGhcAST ()

storeParsed :: StoreParams -> Located (HsModule GhcPs) -> IO ()
storeParsed (StoreParams isVerbose conn (moduleName, moduleId)) md = do
    context <- defaultStoreContext conn moduleId moduleName
    ((), defs) <- runWriterT (runReaderT (store md) context)
    when isVerbose $ do
      putStrLn "### Storing ast definitions:"
      mapM_ print defs
    astIds <- persistAst conn (map convertLocation defs)
    persistDefinitions conn (map convertDefinition (defs `zip` astIds))
    -- TODO: persist comments
  where
    convertLocation (ParseDefinitionRecord _ (NodePos startRow startColumn endRow endColumn))
      = (moduleId, startRow, startColumn, endRow, endColumn)
    convertDefinition ((ParseDefinitionRecord kind _), astId)
      = (moduleId, astId, fromEnum kind)

storeNames :: StoreParams -> HsGroup GhcRn -> IO ()
storeNames (StoreParams isVerbose conn (moduleName, moduleId)) gr = do
    context <- defaultStoreContext conn moduleId moduleName
    ((), names) <- runWriterT (runReaderT (store gr) context)
    let uniqueNames = nub $ sort names
    when isVerbose $ do
      putStrLn "### Storing names:"
      mapM_ print uniqueNames
    persistNames conn moduleId uniqueNames

persistNames :: Connection -> Int -> [NameRecord] -> IO ()
persistNames conn moduleId names = do
    astIds <- persistAst conn (map convertLocation names)
    persistName conn (map convertName (names `zip` astIds))
  where
    convertName ((NameRecord name namespace isDefined definition _), id) = (moduleId, id :: Int, name, fmap fromEnum namespace, isDefined, fmap npStartRow definition, fmap npStartCol definition)
    convertLocation (NameRecord { nmPos = NodePos startRow startColumn endRow endColumn })
      = (moduleId, startRow, startColumn, endRow, endColumn)

storeTypes :: StoreParams -> TcGblEnv -> IO ()
storeTypes (StoreParams isVerbose conn (moduleName, moduleId)) env = do
    context <- defaultStoreContext conn moduleId moduleName
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
    context <- defaultStoreContext conn moduleId moduleName
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
    convertName (NameAndTypeRecord { ntrName, ntrNamespace, ntrIsDefined }, id) = (moduleId, id :: Int, ntrName, fmap fromEnum ntrNamespace, ntrIsDefined, Nothing, Nothing)
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
