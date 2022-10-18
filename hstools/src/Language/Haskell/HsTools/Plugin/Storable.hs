{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.HsTools.Plugin.Storable where

import Control.Monad.Reader
import Control.Monad.Writer

import ConLike
import DataCon
import HsExtension
import HsSyn
import Module
import Name
import PatSyn
import TyCon
import TyCoRep
import SrcLoc
import Var

import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.Plugin.Monad
import Language.Haskell.HsTools.Plugin.Types

type StorablePass p r =
  ( Storable (IdP (GhcPass p)) r
  , Storable (XExprWithTySig (GhcPass p)) r
  , Storable (XSigPat (GhcPass p)) r
  , Storable (XCFieldOcc (GhcPass p)) r
  , Storable (XUnambiguous (GhcPass p)) r
  , Storable (XAppTypeE (GhcPass p)) r
  , Storable (XCFieldOcc (GhcPass p)) r
  , Storable (NameOrRdrName (IdP (GhcPass p))) r
  , StoreMode r
  )

type StoreMode r =
  ( Storable Name r
  , Storable Id r
  , Storable TypedName r
  , Storable ModuleName r
  , CanStoreDefinition r
  )

data TypedName = TN { tnName :: Name, tnType :: Type }

class Storable a r where
    store :: a -> StoreM r ()

class HasTypedName a where
  getTypedName :: a -> Maybe TypedName

instance HasTypedName Id where
  getTypedName id = Just $ TN (Var.varName id) (varType id) 

instance HasTypedName TyThing where
  getTypedName (AnId id) = getTypedName id
  getTypedName (AConLike conLike) = getTypedName conLike
  getTypedName (ATyCon tycon) = getTypedName tycon
  getTypedName (ACoAxiom _) = Nothing

instance HasTypedName ConLike where
  getTypedName (RealDataCon dataCon) = getTypedName dataCon
  getTypedName (PatSynCon patSyn) = getTypedName patSyn

instance HasTypedName TyCon where
  getTypedName tc = Just $ TN (tyConName tc) (tyConKind tc) 

instance HasTypedName PatSyn where
  getTypedName _ps = Nothing

instance HasTypedName DataCon where
  getTypedName dc = Just $ TN (dataConName dc) (dataConRepType dc) 

class CanStoreDefinition r where
  storeDefinition :: DefinitionKind -> StoreM r ()

instance CanStoreDefinition ParseRecord where
  storeDefinition k = srcSpanToNodePos <$> asks scSpan >>= \case 
    Just np -> tell [ ParseDefinitionRecord k np ]
    Nothing -> return ()

instance CanStoreDefinition NameRecord where
  storeDefinition _ = return ()

instance CanStoreDefinition TypeRecord where
  storeDefinition _ = return ()

instance CanStoreDefinition NameAndTypeRecord where
  storeDefinition _ = return ()

storeDefCtx :: CanStoreDefinition r => DefinitionKind -> StoreM r () -> StoreM r ()
storeDefCtx DefParameter st = do
  currDef <- currentDefinition
  case currDef of
    Just d | isSignatureDef (dcKind d) -> storeDefinition DefParameter >> definitionContext DefParameter st
    _ -> st
storeDefCtx k st = storeDefinition k >> definitionContext k st

storeLoc :: (a -> StoreM r ()) -> Located a -> StoreM r ()
storeLoc st (L span ast) = {- trace ("##L: " ++ show span) $ -} do
  thSpan <- case srcSpanToNodePos span of
              Just np -> asks (any (\sp -> sp `containsNP` np) . scThSpans)
              Nothing -> return False
  when (not thSpan) $ (if isGoodSrcSpan span then withSpan span . applyContext else id) $ st ast
