{-# LANGUAGE RecordWildCards #-}

module Language.Haskell.HsTools.Plugin.Monad where

import Control.Monad.Reader
import Control.Monad.Writer
import Database.PostgreSQL.Simple (Connection)

import SrcLoc

import Language.Haskell.HsTools.Plugin.Types
import Language.Haskell.HsTools.Database

type StoreM r = ReaderT StoreContext (WriterT [r] IO)

data StoreContext = StoreContext
  { scModuleName :: String
  , scSpan :: SrcSpan
  , scDefining :: Bool
  , scDefinition :: DefinitionContext
  , scThSpans :: [NodePos]
  }

defining :: StoreM r a -> StoreM r a
defining = local (\l -> l { scDefining = True })

withSpan :: SrcSpan -> StoreM r a -> StoreM r a
withSpan span = local (\l -> l { scSpan = span })

definitionContext :: DefinitionContext -> StoreM r a -> StoreM r a
definitionContext def = local (\l -> l { scDefinition = def })

data DefinitionContext = Root | InstanceDefinition
  deriving Eq

defaultStoreContext :: Connection -> Int -> String -> IO StoreContext
defaultStoreContext conn moduleId moduleName = do
  thSpans <- getTHRanges conn moduleId
  return $ StoreContext
    { scModuleName = moduleName
    , scSpan = noSrcSpan
    , scDefining = False
    , scDefinition = Root
    , scThSpans = map (\(npStartRow, npStartCol, npEndRow, npEndCol) -> NodePos {..}) thSpans
    }
