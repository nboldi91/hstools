{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Haskell.HsTools.Plugin.Monad where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe

import SrcLoc

import Language.Haskell.HsTools.Plugin.Types
import Language.Haskell.HsTools.Utils
import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.SourcePosition

type StoreM r = ReaderT (StoreContext r) (WriterT [r] IO)

type PersistStageM = ReaderT DbConn IO

type StoreStageM = ReaderT StoreParams IO

data StoreContext r = StoreContext
  { scModuleName :: String
  , scSpan :: SrcSpan
  , scDefining :: Bool
  , scDefinitions :: [DefinitionContext]
  , scInsideDefinition :: Bool
  , scLocalUnderLoc :: StoreM r () -> StoreM r ()
  , scThSpans :: [Range NodePos]
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

getDefinitionPosition :: StoreM r (Maybe (Range NodePos))
getDefinitionPosition = do
  isInsideDefinition <- asks scInsideDefinition
  definition <- currentDefinition
  defined <- asks scDefining
  return $ guard (defined && not isInsideDefinition) >> definition >>= srcSpanToNodePos . dcSpan

currentPos :: (Range NodePos -> StoreM r ()) -> StoreM r ()
currentPos st = do
  span <- asks scSpan
  case srcSpanToNodePos span of
    Just np -> st np
    Nothing -> return ()

defaultStoreContext :: Maybe SrcSpan -> StoreStageM (StoreContext r)
defaultStoreContext modSpan = do
  (moduleName, moduleId) <- asks spModule
  thSpans <- withReaderT storeParamsDbConn $ getTHRanges moduleId
  return $ StoreContext
    { scModuleName = moduleName
    , scSpan = noSrcSpan
    , scDefining = False
    , scDefinitions = maybe [] (\ms -> [DefinitionContext DefModule ms]) modSpan
    , scThSpans = map (\(npStartRow, npStartCol, npEndRow, npEndCol) -> Range (SP npStartRow npStartCol) (SP npEndRow npEndCol)) thSpans
    , scLocalUnderLoc = id
    , scInsideDefinition = False
    }

instance DBMonad PersistStageM where
  getConnection = asks dbConnConnection
  logOperation s = do
    isLogging <- asks (\c -> logOptionsQueries (dbConnLogOptions c) || logOptionsPerformance (dbConnLogOptions c))
    when isLogging $ liftIO $ putStrLn s
  logPerformance s = do
    isLogging <- asks (logOptionsPerformance . dbConnLogOptions)
    when isLogging $ liftIO $ putStrLn s
  shouldLogFullData = asks (logOptionsFullData . dbConnLogOptions)
