{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.HsTools.Plugin.Monad where

import Control.Monad.Reader
import Control.Monad.Writer
import Database.PostgreSQL.Simple (Connection)
import Data.Maybe

import SrcLoc

import Language.Haskell.HsTools.Plugin.Types
import Language.Haskell.HsTools.Database

type StoreM r = ReaderT (StoreContext r) (WriterT [r] IO)

data StoreContext r = StoreContext
  { scModuleName :: String
  , scSpan :: SrcSpan
  , scDefining :: Bool
  , scDefinitions :: [DefinitionContext]
  , scInsideDefinition :: Bool
  , scLocalUnderLoc :: StoreM r () -> StoreM r ()
  , scThSpans :: [NodePos]
  }

pushDownContext :: (StoreM r () -> StoreM r ()) -> StoreM r () -> StoreM r ()
pushDownContext ctxChange = local (\l -> l { scLocalUnderLoc = ctxChange . scLocalUnderLoc l })

applyContext :: StoreM r () -> StoreM r ()
applyContext st = do 
  f <- asks scLocalUnderLoc
  f $ local (\l -> l { scLocalUnderLoc = id }) st

defining :: StoreM r a -> StoreM r a
defining = local (\l -> l { scDefining = True })

withSpan :: SrcSpan -> StoreM r a -> StoreM r a
withSpan span = local (\l -> l { scSpan = span })

insideDefinition :: StoreM r a -> StoreM r a
insideDefinition = local (\l -> l { scInsideDefinition = True })

definitionContext :: DefinitionKind -> StoreM r a -> StoreM r a
definitionContext def = local (\l -> l { scDefinitions = DefinitionContext def (scSpan l) : scDefinitions l, scInsideDefinition = False })

inInstanceDefinition :: StoreM r Bool
inInstanceDefinition = asks (any ((== DefInstance) . dcKind) . scDefinitions)

currentDefinition :: StoreM r (Maybe DefinitionContext)
currentDefinition = asks (listToMaybe . scDefinitions)

data DefinitionContext = DefinitionContext { dcKind :: DefinitionKind, dcSpan :: SrcSpan }
  deriving Eq

defaultStoreContext :: Connection -> Int -> String -> IO (StoreContext r)
defaultStoreContext conn moduleId moduleName = do
  thSpans <- getTHRanges conn moduleId
  return $ StoreContext
    { scModuleName = moduleName
    , scSpan = noSrcSpan
    , scDefining = False
    , scDefinitions = []
    , scThSpans = map (\(npStartRow, npStartCol, npEndRow, npEndCol) -> NodePos {..}) thSpans
    , scLocalUnderLoc = id
    , scInsideDefinition = False
    }
