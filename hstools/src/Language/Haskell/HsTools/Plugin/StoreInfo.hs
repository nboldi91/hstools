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
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

import ApiAnnotation
import Bag
import CoreSyn
import HsBinds
import HsDecls
import HscTypes
import HsExpr
import HsExtension
import HsImpExp
import HsTypes
import qualified Class as GHC
import InstEnv (instanceDFunId)
import Id
import Name
import OccName (occNameString)
import Outputable
import SrcLoc
import TcEvidence
import TcRnTypes
import TcType (tcSplitSigmaTy)
import Type (Type, getClassPredTys_maybe, splitTyConApp_maybe)
import UniqFM

import Language.Haskell.HsTools.Plugin.Monad
import Language.Haskell.HsTools.Plugin.Types
import Language.Haskell.HsTools.Plugin.Storable
import Language.Haskell.HsTools.Plugin.StorableInstances (generateFullName)
import Language.Haskell.HsTools.Plugin.StoreComments
import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.SourcePosition (Range(..), SP(..))
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

-- | Extract and persist instance declarations from the renamed AST
storeInstances :: HsGroup GhcRn -> StoreStageM ()
storeInstances (HsGroup _ _ _ tyclGroups derivDecls _ _ _ _ _ _ _) = do
  StoreParams logOptions _ (_, moduleId) <- ask
  let instDecls = concatMap extractInstDeclsFromGroup tyclGroups
      derivs = map extractDerivDecl derivDecls
      derivingClauses = concatMap extractDerivingClauses tyclGroups
      allInstances = instDecls ++ derivs ++ derivingClauses
  when (logOptionsFullData logOptions) $ liftIO $ do
    putStrLn "Storing instances:"
    mapM_ print allInstances
  withReaderT storeParamsDbConn $
    persistInstances moduleId allInstances

storeInstances (XHsGroup _) = return ()

-- | Extract instance declarations from a TyClGroup
extractInstDeclsFromGroup :: TyClGroup GhcRn -> [InstanceRecord]
extractInstDeclsFromGroup (TyClGroup _ _ _ insts) = concatMap extractInstDecls insts
extractInstDeclsFromGroup _ = []

-- | Extract instance info from a class instance declaration
extractInstDecls :: LInstDecl GhcRn -> [InstanceRecord]
extractInstDecls (L loc (ClsInstD _ (ClsInstDecl _ ty _ _ _ _ _))) =
  case extractClassAndType (hsib_body ty) of
    Just (className, typeName, ctx) ->
      case srcSpanToNodePos loc of
        Just pos -> [InstanceRecord className typeName HandWrittenInstance pos ctx]
        Nothing -> []
    Nothing -> []
extractInstDecls _ = []

-- | Extract instance info from a standalone deriving declaration
extractDerivDecl :: LDerivDecl GhcRn -> InstanceRecord
extractDerivDecl (L loc (DerivDecl _ ty _ _)) =
  let (className, typeName, ctx) = fromMaybe ("", "", []) $ extractClassAndType (hsib_body $ hswc_body ty)
      pos = fromMaybe (Range (SP 0 0) (SP 0 0)) $ srcSpanToNodePos loc
  in InstanceRecord className typeName StandaloneDerivedInstance pos ctx
extractDerivDecl (L loc _) =
  let pos = fromMaybe (Range (SP 0 0) (SP 0 0)) $ srcSpanToNodePos loc
  in InstanceRecord "" "" StandaloneDerivedInstance pos []

-- | Extract deriving clause instances from type declarations
extractDerivingClauses :: TyClGroup GhcRn -> [InstanceRecord]
extractDerivingClauses (TyClGroup _ tyclds _ _) = concatMap extractFromTyClDecl tyclds
extractDerivingClauses _ = []

extractFromTyClDecl :: LTyClDecl GhcRn -> [InstanceRecord]
extractFromTyClDecl (L _ (DataDecl _ name _ _ (HsDataDefn _ _ _ _ _ _ derivs))) =
  let typeName = showSDocUnsafe (ppr (unLoc name))
  in concatMap (extractFromDerivClause typeName) (unLoc derivs)
extractFromTyClDecl _ = []

extractFromDerivClause :: String -> LHsDerivingClause GhcRn -> [InstanceRecord]
extractFromDerivClause typeName (L loc (HsDerivingClause _ _ (L _ tys))) =
  let pos = fromMaybe (Range (SP 0 0) (SP 0 0)) $ srcSpanToNodePos loc
  in map (derivedClassToRecord typeName pos) (map hsib_body tys)
extractFromDerivClause _ _ = []

derivedClassToRecord :: String -> Range NodePos -> LHsType GhcRn -> InstanceRecord
derivedClassToRecord typeName pos ty =
  let className = showSDocUnsafe (ppr ty)
  in InstanceRecord className typeName DerivedInstance pos []

-- | Decompose an instance type into (className, typeName, context)
-- e.g., for "Eq a => Ord (Wrapper a)" returns ("Ord", "Wrapper a", [("Eq", "a")])
extractClassAndType :: LHsType GhcRn -> Maybe (String, String, [(String, String)])
extractClassAndType (L _ (HsForAllTy _ _ body)) = extractClassAndType body
extractClassAndType (L _ (HsQualTy _ ctx body)) =
  case extractClassAndType body of
    Just (cls, typ, _) -> Just (cls, typ, extractContext ctx)
    Nothing -> Nothing
extractClassAndType (L _ (HsAppTy _ lhs rhs)) =
  Just (showSDocUnsafe (ppr (unLoc lhs)), showSDocUnsafe (ppr (unLoc rhs)), [])
extractClassAndType (L _ (HsParTy _ inner)) = extractClassAndType inner
extractClassAndType (L _ (HsTyVar _ _ name)) =
  Just (showSDocUnsafe (ppr (unLoc name)), "", [])
extractClassAndType _ = Nothing

-- | Extract context constraints as (className, typeName) pairs
extractContext :: LHsContext GhcRn -> [(String, String)]
extractContext (L _ ctxTypes) = mapMaybe extractConstraint ctxTypes

extractConstraint :: LHsType GhcRn -> Maybe (String, String)
extractConstraint (L _ (HsAppTy _ cls typ)) =
  Just (showSDocUnsafe (ppr (unLoc cls)), showSDocUnsafe (ppr (unLoc typ)))
extractConstraint (L _ (HsParTy _ inner)) = extractConstraint inner
extractConstraint _ = Nothing

-- | Persist instance records to the database
persistInstances :: Int -> [InstanceRecord] -> PersistStageM ()
persistInstances moduleId instances = do
  forM_ instances $ \(InstanceRecord cls typ kind pos ctx) -> do
    let fullRange = FullRange moduleId pos
    [astId] <- persistAst [fullRange]
    instId <- persistInstance moduleId astId cls typ kind
    when (not $ null ctx) $
      persistInstanceDeps (map (\(rc, rt) -> (instId, rc, rt)) ctx)

-- | Extract and persist instance dependencies from the typechecked AST.
-- For each instance definition (AbsBinds with DFunId export), extract
-- DFun references from its evidence bindings and method implementations.
-- These represent the instances required by this instance (e.g. Eq Inner for Eq Outer).
--
-- Example: given the source
--
-- @
--   data Inner = Inner deriving (Eq)
--   data Outer = Outer Inner deriving (Eq)
-- @
--
-- GHC generates top-level bindings in tcg_binds (simplified):
--
-- @
--   $fEqInner :: Eq Inner             -- DFun AbsBinds
--   $c==_Inner :: Inner -> Inner -> Bool  -- method, no DFun evidence
--   $fEqOuter :: Eq Outer             -- DFun AbsBinds
--   $c==_Outer :: Outer -> Outer -> Bool  -- method, evidence refs $fEqInner
-- @
--
-- This function groups $c bindings with their preceding DFun by position,
-- then extracts DFun references from the method evidence. For $c==_Outer,
-- the evidence contains $fEqInner, so we store:
--
--   instance_deps: (instanceId of Eq Outer, requiredClass="Eq", requiredType="Inner")
--
-- Additionally, it inspects tcg_insts (ClsInst) for context-based deps.
-- For parameterized types like @data Box a = Box a deriving (Eq)@, GHC gives
-- the DFun type @Eq a => Eq (Box a)@. The @Eq a@ constraint is recorded so
-- that when the recursive CTE in getUnusedInstances walks instance_deps,
-- concrete instantiations (e.g. Eq Fruit for Box Fruit) can be traced.
storeInstanceDepsFromTc :: TcGblEnv -> StoreStageM ()
storeInstanceDepsFromTc env = do
  StoreParams _ _ (_, moduleId) <- ask
  -- Extract deps from evidence in $c-prefixed method bindings
  depsPerInstance <- liftIO $ extractInstanceDeps (tcg_binds env)
  forM_ depsPerInstance $ \(defDFun, depRecords) -> do
    let InstanceUsageRecord defCls defTyp = dfunToUsageRecord defDFun
    maybeInstId <- withReaderT storeParamsDbConn $ lookupInstanceId moduleId defCls defTyp
    case maybeInstId of
      Just instId -> do
        let newDeps = [ (instId, iuClassName r, iuTypeName r)
                      | r <- depRecords
                      , iuClassName r /= defCls || iuTypeName r /= defTyp  -- skip self-refs
                      ]
        when (not $ null newDeps) $
          withReaderT storeParamsDbConn $ persistInstanceDeps newDeps
      Nothing -> return ()
  -- Extract deps from instance contexts in tcg_insts (covers parameterized derived instances)
  forM_ (tcg_insts env) $ \clsInst -> do
    let dfunId = instanceDFunId clsInst
        InstanceUsageRecord defCls defTyp = dfunToUsageRecord dfunId
        instTy = idType dfunId
        (_, theta, _) = tcSplitSigmaTy instTy
        -- theta contains the constraints (e.g. [Show a] for Show a => Show (Box a))
        -- But these are just class constraints on type variables, not concrete deps.
        -- We need to look at the superclasses differently.
        -- For derived instances, theta may contain concrete constraints like Eq Inner
        contextDeps = concatMap extractDepsFromPred theta
    maybeInstId <- withReaderT storeParamsDbConn $ lookupInstanceId moduleId defCls defTyp
    case maybeInstId of
      Just instId -> do
        let newDeps = [ (instId, cls, typ)
                      | (cls, typ) <- contextDeps
                      , cls /= defCls || typ /= defTyp
                      ]
        when (not $ null newDeps) $
          withReaderT storeParamsDbConn $ persistInstanceDeps newDeps
      Nothing -> return ()

-- | Extract (className, typeName) from a class predicate type
extractDepsFromPred :: Type -> [(String, String)]
extractDepsFromPred pred = case getClassPredTys_maybe pred of
  Just (cls, [ty]) -> [(showSDocUnsafe $ ppr $ GHC.className cls, extractHeadTyCon ty)]
  Just (cls, tys) -> [(showSDocUnsafe $ ppr $ GHC.className cls, showSDocUnsafe $ ppr tys)]
  Nothing -> []

-- | For each instance-definition AbsBinds, extract (definingDFunId, [dependencyRecords]).
-- In GHC 8.6, the method implementations ($c-prefixed bindings) are top-level siblings,
-- not nested inside the DFun AbsBinds. Their evidence bindings reference both the parent
-- DFun and any dependency DFuns. We collect all DFun references from $c-prefixed evidence
-- and associate them with the parent DFun.
extractInstanceDeps :: LHsBinds GhcTc -> IO [(Id, [InstanceUsageRecord])]
extractInstanceDeps binds = do
  let allBinds = map unLoc (bagToList binds)
      -- Walk binds in order, associating each $c binding with the most recent DFun
      grouped = groupMethodsWithDFun allBinds
  -- For each DFun, collect DFun references from its $c methods' evidence
  -- that are NOT the parent DFun itself (those are deps)
  results <- forM grouped $ \(dfunId, methodBinds) -> do
    let parentRec = dfunToUsageRecord dfunId
    deps <- fmap concat $ forM methodBinds $ \case
      AbsBinds _ _ _ _ evBinds _ _ -> do
        evUsages <- fmap concat $ mapM extractUsagesFromTcEvBinds evBinds
        return $ filter (/= parentRec) evUsages
      _ -> return []
    return (dfunId, Set.toList $ Set.fromList deps)
  return [ r | r@(_, deps) <- results, not (null deps) ]

-- | Walk binds in order and associate each $c-prefixed binding with
-- the most recently seen DFun binding.
groupMethodsWithDFun :: [HsBind GhcTc] -> [(Id, [HsBind GhcTc])]
groupMethodsWithDFun = go Nothing []
  where
    go currentDFun acc [] = flush currentDFun acc
    go currentDFun acc (b:bs) = case getDFunExport b of
      Just dfunId ->
        flush currentDFun acc ++ go (Just dfunId) [] bs
      Nothing
        | isMethodBind b -> go currentDFun (acc ++ [b]) bs
        | otherwise -> go currentDFun acc bs
    flush (Just dfun) methods = [(dfun, methods)]
    flush Nothing _ = []
    getDFunExport (AbsBinds _ _ _ exports _ _ _) =
      case find (isDFunId . abe_poly) exports of
        Just e -> Just (abe_poly e)
        Nothing -> Nothing
    getDFunExport _ = Nothing
    isMethodBind (AbsBinds _ _ _ exports _ _ _) =
      any (isInstanceMethodExport . abe_poly) exports
    isMethodBind _ = False

-- | Extract and persist instance usage evidence from the typechecked AST.
-- In GHC 8.6, evidence for instance resolution is embedded inside
-- tcg_binds (the typechecked HsExpr trees) rather than tcg_ev_binds.
-- We also check tcg_ev_binds for any top-level evidence.
storeInstanceUsages :: TcGblEnv -> StoreStageM ()
storeInstanceUsages env = do
  StoreParams logOptions _ (_, moduleId) <- ask
  -- Build raw resolution map from tcg_ev_binds: for each evidence binding,
  -- collect direct DFun usages and evidence variable references
  let evBindsList = bagToList (tcg_ev_binds env)
      rawMap = Map.fromList
        [ (eb_lhs b, (extractUsageFromEvBind b, collectEvVarsFromEvBind b))
        | b <- evBindsList
        ]
      -- Transitively resolve: for each ev var, collect all DFun usages reachable
      resolveAll visited var = case Map.lookup var rawMap of
        _ | Set.member var visited -> []
        Just (usages, evRefs) ->
          usages ++ concatMap (resolveAll (Set.insert var visited)) (Set.toList evRefs)
        Nothing -> []
      evResolutionMap = Map.fromList
        [ (v, resolveAll Set.empty v)
        | v <- map eb_lhs evBindsList
        ]
  -- Extract DFunId usages and evidence variable IDs from non-instance bindings
  (tcBindUsages, evVarIds) <- liftIO $ extractDFunIdsAndEvVars (tcg_binds env)
  -- Resolve evidence variables to DFun usage records
  let resolvedUsages = concatMap (\v -> fromMaybe [] (Map.lookup v evResolutionMap)) (Set.toList evVarIds)
  let usages = tcBindUsages ++ resolvedUsages
      uniqueUsages = Set.toList $ Set.fromList usages
  when (logOptionsFullData logOptions) $ liftIO $ do
    putStrLn "Storing instance usages:"
    mapM_ print uniqueUsages
  when (not $ null uniqueUsages) $
    withReaderT storeParamsDbConn $
      persistInstanceUsages (map (\(InstanceUsageRecord cls typ) -> (moduleId, cls, typ)) uniqueUsages)

-- | Check if an Id is a class method implementation (e.g. $cshow, $c==)
-- These are generated for instance method definitions and reference the parent DFun
isInstanceMethodExport :: Id -> Bool
isInstanceMethodExport v = case nameOccName (getName v) of
  occ -> "$c" `isPrefixOf` occNameString occ

-- | Extract DFunId usage records AND evidence variable IDs from non-instance bindings.
-- Returns (dfunUsages, evVarIds) where evVarIds are evidence variables
-- referenced by non-instance code that need to be resolved via tcg_ev_binds.
extractDFunIdsAndEvVars :: LHsBinds GhcTc -> IO ([InstanceUsageRecord], Set.Set Id)
extractDFunIdsAndEvVars binds = do
  results <- mapM (extractFromBind . unLoc) (bagToList binds)
  let (usages, vars) = unzip results
  return (concat usages, Set.unions vars)
  where
    extractFromBind (AbsBinds _ _ _ exports evBinds innerBinds _)
      | any (isDFunId . abe_poly) exports = return ([], Set.empty)
      | any (isInstanceMethodExport . abe_poly) exports = return ([], Set.empty)
      | otherwise = do
        evUsages <- fmap concat $ mapM extractUsagesFromTcEvBinds evBinds
        (innerUsages, innerVars) <- extractDFunIdsAndEvVars innerBinds
        return (evUsages ++ innerUsages, innerVars)
    extractFromBind (FunBind _ _ mg _ _) = extractFromMG mg
    extractFromBind (PatBind _ _ rhs _) = extractFromGRHSs rhs
    extractFromBind _ = return ([], Set.empty)
    extractFromMG (MG _ (L _ matches) _) = do
      results <- mapM (extractFromMatch . unLoc) matches
      let (u, v) = unzip results in return (concat u, Set.unions v)
    extractFromMG _ = return ([], Set.empty)
    extractFromMatch (Match _ _ _ rhs) = extractFromGRHSs rhs
    extractFromMatch _ = return ([], Set.empty)
    extractFromGRHSs (GRHSs _ grhss _) = do
      results <- mapM (extractFromGRHS . unLoc) grhss
      let (u, v) = unzip results in return (concat u, Set.unions v)
    extractFromGRHSs _ = return ([], Set.empty)
    extractFromGRHS (GRHS _ _ body) = extractFromLExpr body
    extractFromGRHS _ = return ([], Set.empty)
    extractFromLExpr (L _ expr) = extractFromExpr expr
    extractFromExpr (HsVar _ (L _ v))
      | isDFunId v = return ([dfunToUsageRecord v], Set.empty)
      | otherwise = return ([], Set.empty)
    extractFromExpr (HsWrap _ wrapper inner) = do
      let (wUsages, wVars) = extractFromWrapper wrapper
      (iUsages, iVars) <- extractFromExpr inner
      return (wUsages ++ iUsages, Set.union wVars iVars)
    extractFromExpr (HsApp _ f a) = do
      (fu, fv) <- extractFromLExpr f
      (au, av) <- extractFromLExpr a
      return (fu ++ au, Set.union fv av)
    extractFromExpr (OpApp _ l op r) = do
      (lu, lv) <- extractFromLExpr l
      (ou, ov) <- extractFromLExpr op
      (ru, rv) <- extractFromLExpr r
      return (lu ++ ou ++ ru, Set.unions [lv, ov, rv])
    extractFromExpr (NegApp _ e _) = extractFromLExpr e
    extractFromExpr (HsPar _ e) = extractFromLExpr e
    extractFromExpr (SectionL _ e1 e2) = do
      (u1, v1) <- extractFromLExpr e1
      (u2, v2) <- extractFromLExpr e2
      return (u1 ++ u2, Set.union v1 v2)
    extractFromExpr (SectionR _ e1 e2) = do
      (u1, v1) <- extractFromLExpr e1
      (u2, v2) <- extractFromLExpr e2
      return (u1 ++ u2, Set.union v1 v2)
    extractFromExpr (ExplicitTuple _ args _) = do
      results <- mapM (\(L _ a) -> case a of Present _ e -> extractFromLExpr e; _ -> return ([], Set.empty)) args
      let (u, v) = unzip results in return (concat u, Set.unions v)
    extractFromExpr (HsCase _ scrut mg) = do
      (su, sv) <- extractFromLExpr scrut
      (mu, mv) <- extractFromMG mg
      return (su ++ mu, Set.union sv mv)
    extractFromExpr (HsIf _ _ c t f) = do
      (cu, cv) <- extractFromLExpr c
      (tu, tv) <- extractFromLExpr t
      (fu, fv) <- extractFromLExpr f
      return (cu ++ tu ++ fu, Set.unions [cv, tv, fv])
    extractFromExpr (HsLet _ (L _ binds) body) = do
      (bu, bv) <- extractFromLocalBinds binds
      (eu, ev) <- extractFromLExpr body
      return (bu ++ eu, Set.union bv ev)
    extractFromExpr (HsDo _ _ (L _ stmts)) = do
      results <- mapM (extractFromStmt . unLoc) stmts
      let (u, v) = unzip results in return (concat u, Set.unions v)
    extractFromExpr (ExplicitList _ _ exprs) = do
      results <- mapM extractFromLExpr exprs
      let (u, v) = unzip results in return (concat u, Set.unions v)
    extractFromExpr (HsLam _ mg) = extractFromMG mg
    extractFromExpr _ = return ([], Set.empty)
    extractFromWrapper (WpEvApp (EvExpr expr)) =
      let usages = extractUsageFromCoreExpr expr
          evVars = collectEvVars expr
      in (usages, evVars)
    extractFromWrapper (WpEvApp _) = ([], Set.empty)
    extractFromWrapper (WpCompose w1 w2) =
      let (u1, v1) = extractFromWrapper w1
          (u2, v2) = extractFromWrapper w2
      in (u1 ++ u2, Set.union v1 v2)
    extractFromWrapper _ = ([], Set.empty)
    extractFromLocalBinds (HsValBinds _ (XValBindsLR (NValBinds binds _))) = do
      results <- mapM (extractDFunIdsAndEvVars . snd) binds
      let (u, v) = unzip results in return (concat u, Set.unions v)
    extractFromLocalBinds _ = return ([], Set.empty)
    extractFromStmt (BindStmt _ _ body _ _) = extractFromLExpr body
    extractFromStmt (BodyStmt _ body _ _) = extractFromLExpr body
    extractFromStmt (LastStmt _ body _ _) = extractFromLExpr body
    extractFromStmt (LetStmt _ (L _ binds)) = extractFromLocalBinds binds
    extractFromStmt _ = return ([], Set.empty)

dfunToUsageRecord :: Id -> InstanceUsageRecord
dfunToUsageRecord v =
  let ty = idType v
      -- strip foralls and constraints to get to the class predicate (e.g. Show Int)
      (_, _, classPred) = tcSplitSigmaTy ty
      (className, typeName) = case getClassPredTys_maybe classPred of
        Just (cls, [ty]) -> (showSDocUnsafe $ ppr $ className' cls, extractHeadTyCon ty)
        Just (cls, tys) -> (showSDocUnsafe $ ppr $ className' cls, showSDocUnsafe $ ppr tys)
        Nothing -> (showSDocUnsafe $ ppr classPred, "")
      className' cls = GHC.className cls
  in InstanceUsageRecord className typeName

-- | Extract the head type constructor name from a type.
-- For "Box a" returns "Box", for "Int" returns "Int", for type variables returns their name.
extractHeadTyCon :: Type -> String
extractHeadTyCon ty = case splitTyConApp_maybe ty of
  Just (tc, _) -> showSDocUnsafe (ppr tc)
  Nothing -> showSDocUnsafe (ppr ty)

-- | Extract instance usage from a single evidence binding
extractUsageFromEvBind :: EvBind -> [InstanceUsageRecord]
extractUsageFromEvBind (EvBind { eb_rhs = term }) = extractUsageFromEvTerm term

-- | Extract instance usage from an evidence term (GHC 8.6)
extractUsageFromEvTerm :: EvTerm -> [InstanceUsageRecord]
extractUsageFromEvTerm (EvExpr expr) = extractUsageFromCoreExpr expr
extractUsageFromEvTerm _ = []

-- | Extract usages from TcEvBinds (supports both pure EvBinds and mutable TcEvBinds)
extractUsagesFromTcEvBinds :: TcEvBinds -> IO [InstanceUsageRecord]
extractUsagesFromTcEvBinds (EvBinds bag) =
  return $ concatMap extractUsageFromEvBind (bagToList bag)
extractUsagesFromTcEvBinds (TcEvBinds ref) = case ref of
  EvBindsVar { ebv_binds = bindsRef } -> do
    bindsMap <- readIORef bindsRef
    return $ concatMap extractUsageFromEvBind (bagToList $ evBindMapBinds bindsMap)
  NoEvBindsVar {} -> return []

-- | Extract instance usages from a Core expression
extractUsageFromCoreExpr :: CoreExpr -> [InstanceUsageRecord]
extractUsageFromCoreExpr (Var v)
  | isDFunId v = [dfunToUsageRecord v]
  | otherwise = []
extractUsageFromCoreExpr (App f a) = extractUsageFromCoreExpr f ++ extractUsageFromCoreExpr a
extractUsageFromCoreExpr _ = []

-- | Collect non-DFun evidence variable references from an EvBind's RHS
collectEvVarsFromEvBind :: EvBind -> Set.Set Id
collectEvVarsFromEvBind (EvBind { eb_rhs = EvExpr expr }) = collectEvVars expr
collectEvVarsFromEvBind _ = Set.empty

-- | Collect non-DFun evidence variables from a Core expression
collectEvVars :: CoreExpr -> Set.Set Id
collectEvVars (Var v)
  | not (isDFunId v) = Set.singleton v
  | otherwise = Set.empty
collectEvVars (App f a) = Set.union (collectEvVars f) (collectEvVars a)
collectEvVars _ = Set.empty
