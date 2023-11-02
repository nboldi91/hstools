{-# LANGUAGE EmptyDataDeriving #-}

module Language.Haskell.HsTools.Plugin.Types where

import Database.PostgreSQL.Simple (Connection)

import Name
import SrcLoc

import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.Utils (LogOptions(..), DbConn(..))
import Language.Haskell.HsTools.SourcePosition (Range(..), SP(..))

data ParseRecord
  = ParseDefinitionRecord
    { prKind :: DefinitionKind
    , prPos :: Range NodePos
    }
  | ParseModuleName
    { prModuleName :: String
    , prPos :: Range NodePos
    , prIsDefined :: Bool
    , prDefinitionOf :: Maybe (Range NodePos)
    }
  deriving (Show, Eq, Ord)

data NameRecord = NameRecord
  { nmName :: FullName
  , nmIsDefined :: Bool
  , nmDefinitionOf :: Maybe (Range NodePos)
  , nmPos :: Range NodePos
  } deriving (Show, Eq, Ord)

data NodePos
  deriving (Show, Eq, Ord)

data TypeRecord = TypeRecord
  { trTypedName :: FullName
  , trType :: String
  } deriving (Show, Eq, Ord)

data NameAndTypeRecord = NameAndTypeRecord
  { ntrName :: FullName
  , ntrIsDefined :: Bool
  , ntrType :: Maybe String
  , ntrPos :: Range NodePos
  } deriving (Show, Eq, Ord)

data StoreParams = StoreParams
  { spLogOptions :: LogOptions
  , spConnection :: Connection
  , spModule :: (String, Int)
  }

storeParamsDbConn :: StoreParams -> DbConn
storeParamsDbConn sp = DbConn (spLogOptions sp) (spConnection sp)

containsNP :: Range NodePos -> Range NodePos -> Bool
Range (SP sr1 sc1) (SP er1 ec1) `containsNP` Range (SP sr2 sc2) (SP er2 ec2)
  = (sr1 < sr2 || (sr1 == sr2 && sc1 <= sc2)) && (er1 > er2 || (er1 == er2 && ec1 >= ec2))

srcSpanToNodePos :: SrcSpan -> Maybe (Range NodePos)
srcSpanToNodePos (RealSrcSpan span) = Just $ realSrcSpanToNodePos span
srcSpanToNodePos _ = Nothing

realSrcSpanToNodePos :: RealSrcSpan -> Range NodePos
realSrcSpanToNodePos span
  = Range (SP (srcLocLine start) (srcLocCol start)) (SP (srcLocLine end) (srcLocCol end))
  where
    start = realSrcSpanStart span
    end = realSrcSpanEnd span

nameNamespace :: Name -> Maybe Namespace
nameNamespace n 
  | isTyVarName n = Just TyVarNS
  | isTyConName n = Just TyConNS
  | isDataConName n = Just DataConNS
  | isValName n = Just ValNS
  | isVarName n = Just VarNS
  | otherwise = Nothing
