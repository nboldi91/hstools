module Language.Haskell.HsTools.Plugin.Types where

import Database.PostgreSQL.Simple (Connection)

import Name
import SrcLoc

import Language.Haskell.HsTools.Database

data ParseRecord = ParseDefinitionRecord
  { prKind :: DefinitionKind
  , prPos :: NodePos
  } deriving (Show, Eq)

data NameRecord = NameRecord
  { nmName :: String
  , nmNamespace :: Maybe Namespace
  , nmIsDefined :: Bool
  , nmDefinitionOf :: Maybe NodePos
  , nmPos :: NodePos
  } deriving (Show, Eq, Ord)

data NodePos = NodePos
  { npStartRow :: Int
  , npStartCol :: Int
  , npEndRow :: Int
  , npEndCol :: Int
  } deriving (Eq, Ord)

data TypeRecord = TypeRecord
  { trTypedName :: String
  , trTypeNamespace :: Maybe Namespace
  , trType :: String
  } deriving (Show, Eq, Ord)

data NameAndTypeRecord = NameAndTypeRecord
  { ntrName :: String
  , ntrNamespace :: Maybe Namespace
  , ntrIsDefined :: Bool
  , ntrType :: Maybe String
  , ntrPos :: NodePos
  } deriving (Show, Eq, Ord)

data StoreParams = StoreParams
  { spIsVerbose :: Bool
  , spConnection :: Connection
  , spModule :: (String, Int)
  }

instance Show NodePos where
  show (NodePos sr sc er ec) = show sr ++ ":" ++ show sc ++ "-" ++ show er ++ ":" ++ show ec

containsNP :: NodePos -> NodePos -> Bool
NodePos sr1 sc1 er1 ec1 `containsNP` NodePos sr2 sc2 er2 ec2
  = (sr1 < sr2 || (sr1 == sr2 && sc1 <= sc2)) && (er1 > er2 || (er1 == er2 && ec1 >= ec2))

srcSpanToNodePos :: SrcSpan -> Maybe NodePos
srcSpanToNodePos (RealSrcSpan span) = Just $ realSrcSpanToNodePos span
srcSpanToNodePos _ = Nothing

realSrcSpanToNodePos :: RealSrcSpan -> NodePos
realSrcSpanToNodePos span
  = (NodePos (srcLocLine start) (srcLocCol start) (srcLocLine end) (srcLocCol end))
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
