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

module Language.Haskell.HsTools.Plugin.PersistNameInfo where

import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe
import Database.PostgreSQL.Simple (Connection)

import HsDecls
import HsExpr
import HsExtension
import SrcLoc
import TcRnTypes

import Language.Haskell.HsTools.Plugin.Monad
import Language.Haskell.HsTools.Plugin.Types
import Language.Haskell.HsTools.Plugin.Class
import Language.Haskell.HsTools.Database

storeTypes :: StoreParams -> TcGblEnv -> IO ()
storeTypes (StoreParams isVerbose conn (moduleName, moduleId)) env = do
    astNodes <- getAstNodes conn moduleId
    let astNodeMap = M.fromList $ map (\(startRow, startColumn, endRow, endColumn, astId) 
                                          -> (NodePos startRow startColumn endRow endColumn, astId)) 
                                      astNodes
    context <- defaultStoreContext conn moduleId moduleName
    ((), types) <- runWriterT (runReaderT storeEnv context{ scNodeMap = astNodeMap })
    when isVerbose $ do
      putStrLn "### Storing types:"
      mapM_ print types
    persistTypes conn (map convertType types)
  where
    convertType (TypeRecord astNode typ) = (moduleId, astNode, typ)
    storeEnv = do
      -- TODO: finish storing types
      store (tcg_binds env)
      --store (tcg_sigs env)
      --store (tcg_imp_specs env)
      --store (tcg_warns env)
      --store (tcg_anns env)
      --store (tcg_tcs env)
      --store (tcg_insts env)
      --store (tcg_fam_insts env)
      --store (tcg_rules env)
      --store (tcg_fords env)
      --store (tcg_patsyns env)
      --store (tcg_doc_hdr env)

storeNames :: StoreParams -> HsGroup GhcRn -> IO ()
storeNames (StoreParams isVerbose conn (moduleName, moduleId)) gr = do
    context <- defaultStoreContext conn moduleId moduleName
    ((), names) <- runWriterT (runReaderT (store gr) context)
    when isVerbose $ do
      putStrLn "### Storing names:"
      mapM_ print names
    persistNames conn moduleId names

persistNames :: Connection -> Int -> [NameRecord] -> IO ()
persistNames conn moduleId names = do
    astIds <- persistAst conn (map convertLocation names)
    persistName conn (map convertName (names `zip` astIds))
  where
    convertName ((NameRecord name namespace isDefined _), id) = (moduleId, id :: Int, name, fmap fromEnum namespace, isDefined)
    convertLocation (NameRecord { nmPos = NodePos startRow startColumn endRow endColumn })
      = (moduleId, startRow, startColumn, endRow, endColumn)

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
    persistTypes conn (catMaybes $ map convertType (namesAndTypes `zip` astIds))
  where
    convertName (NameAndTypeRecord { ntrName, ntrNamespace, ntrIsDefined }, id) = (moduleId, id :: Int, ntrName, fmap fromEnum ntrNamespace, ntrIsDefined)
    convertType (NameAndTypeRecord { ntrType }, id) = fmap (moduleId, id :: Int,) ntrType
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
