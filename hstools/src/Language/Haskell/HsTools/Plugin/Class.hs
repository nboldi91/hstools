{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.HsTools.Plugin.Class where

import Control.Monad.Reader
import Control.Monad.Writer

import Bag
import BooleanFormula
import ConLike
import DataCon
import HsBinds
import HsDecls
import HsExpr
import HsExtension
import HsPat
import HsTypes
import HsSyn
import IdInfo
import Module
import Name
import Outputable
import RdrName
import PatSyn
import SrcLoc
import TyCon
import TyCoRep
import TysWiredIn
import Unique
import Var

import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.Plugin.Monad
import Language.Haskell.HsTools.Plugin.Types

type IsGhcPass p r =
  ( HaskellAst Name r -- skip
  , HaskellAst Id r -- skip
  , HaskellAst TypedName r
  , CanStoreDefinition r
  , HaskellAst (IdP (GhcPass p)) r
  , HaskellAst (XExprWithTySig (GhcPass p)) r
  , HaskellAst (XSigPat (GhcPass p)) r
  , HaskellAst (XCFieldOcc (GhcPass p)) r
  , HaskellAst (XUnambiguous (GhcPass p)) r
  , HaskellAst (XAppTypeE (GhcPass p)) r
  , HaskellAst (XCFieldOcc (GhcPass p)) r
  , HaskellAst (NameOrRdrName (IdP (GhcPass p))) r
  )

data TypedName = TN { tnName :: Name, tnType :: Type }

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

class HaskellAst a r where
    store :: a -> StoreM r ()

instance HaskellAst RdrName ParseRecord where
  store _ = return ()

instance HaskellAst Name NameRecord where
  store id = asks scSpan >>= \case
    RealSrcSpan span -> do
      currentModuleName <- asks scModuleName
      definition <- currentDefinition
      defined <- asks scDefining
      let storedName = NameRecord
            { nmName = generateFullName currentModuleName id
            , nmNamespace = nameNamespace id
            , nmIsDefined = defined
            , nmDefinitionOf = guard defined >> definition >>= srcSpanToNodePos . dcSpan
            , nmPos = realSrcSpanToNodePos span
            }
      tell [storedName] 
    _ -> liftIO $ putStrLn $ "WARNING " ++ (showSDocUnsafe $ ppr id) ++ " does not have a real src span"

instance HaskellAst TypedName ParseRecord where
  store _ = return ()

instance HaskellAst TypedName TypeRecord where
  store (TN name typ) = do
    currentModuleName <- asks scModuleName
    tell [ nameTypeToTypeRecord currentModuleName name typ ]

instance HaskellAst TypedName NameRecord where
  store (TN name _) = store name

instance HaskellAst TypedName NameAndTypeRecord where
  store (TN name typ) = do 
    span <- asks scSpan
    case srcSpanToNodePos span of
      Just np -> do
        currentModuleName <- asks scModuleName
        defined <- asks scDefining
        let typeStr = showSDocUnsafe $ ppr typ
        let modName = nameModule_maybe name
            fullName = case modName of
                        Just mn -> showSDocUnsafe (pprModule mn) ++ "." ++ showSDocUnsafe (ppr name)
                        Nothing -> currentModuleName ++ "." ++ showSDocUnsafe (ppr name) ++ ":" ++ showSDocUnsafe (pprUniqueAlways (getUnique name)) 
            record = NameAndTypeRecord
                { ntrName = fullName
                , ntrNamespace = nameNamespace name
                , ntrIsDefined = defined
                , ntrType = Just typeStr
                , ntrPos = np
                }
        tell [record] 
      Nothing -> return ()
  
instance HaskellAst Name ParseRecord where
  store _ = return ()

instance HaskellAst Name TypeRecord where
  store _ = return ()

instance HaskellAst Id ParseRecord where
  store _ = return ()

instance HaskellAst Id NameRecord where
  store = store . getTypedName

instance HaskellAst Id TypeRecord where
  store id =
    store $ case idDetails id of
      DataConWorkId dc -> getTypedName dc
      _ -> getTypedName id

instance HaskellAst Id NameAndTypeRecord where
  store id =
    store $ case idDetails id of
      DataConWorkId dc -> getTypedName dc
      _ -> getTypedName id

nameTypeToTypeRecord :: String -> Name -> Type -> TypeRecord
nameTypeToTypeRecord currentModuleName name typ
  = TypeRecord
      (generateFullName currentModuleName name)
      (nameNamespace name)
      (showSDocUnsafe $ ppr typ)

generateFullName :: String -> Name -> String
generateFullName currentModuleName name = case nameModule_maybe name of
  (Just mn) -> showSDocUnsafe (pprModule mn) ++ "." ++ showSDocUnsafe (ppr name)
  Nothing -> currentModuleName ++ "." ++ showSDocUnsafe (ppr name) ++ ":" ++ showSDocUnsafe (pprUniqueAlways (getUnique name)) 

instance HaskellAst Name NameAndTypeRecord where
  store id = do 
    span <- asks scSpan
    case srcSpanToNodePos span of
      Just np -> do
        currentModuleName <- asks scModuleName
        defined <- asks scDefining
        let modName = nameModule_maybe id
            fullName = case modName of
                        Just mn -> showSDocUnsafe (pprModule mn) ++ "." ++ showSDocUnsafe (ppr id)
                        Nothing -> currentModuleName ++ "." ++ showSDocUnsafe (ppr id) ++ ":" ++ showSDocUnsafe (pprUniqueAlways (getUnique id)) 
            record = NameAndTypeRecord
                { ntrName = fullName
                , ntrNamespace = nameNamespace id
                , ntrIsDefined = defined
                , ntrType = Nothing
                , ntrPos = np
                }
        tell [record] 
      Nothing -> return ()

instance HaskellAst Type r where
  store _ = return ()

instance HaskellAst NoExt r where
  store _ = return ()

instance HaskellAst a r => HaskellAst (Located a) r where
  store = storeLoc store

storeLoc :: (a -> StoreM r ()) -> Located a -> StoreM r ()
storeLoc st (L span ast) = {- trace ("##L: " ++ show span) $ -} do
  thSpan <- case srcSpanToNodePos span of
              Just np -> asks (any (\sp -> sp `containsNP` np) . scThSpans)
              Nothing -> return False
  when (not thSpan) $ (if isGoodSrcSpan span then withSpan span else id) $ st ast

instance HaskellAst a r => HaskellAst [a] r where
    store = mapM_ store

instance HaskellAst a r => HaskellAst (Bag a) r where
    store = mapBagM_ store

instance HaskellAst a r => HaskellAst (Maybe a) r where
    store (Just a) = store a
    store Nothing = return ()

instance (HaskellAst a r, HaskellAst b r) => HaskellAst (a, b) r where
    store (a, b) = store a >> store b

instance HaskellAst a r => HaskellAst (HsWildCardBndrs (GhcPass p) a) r where
    store = store . hswc_body

instance HaskellAst a r => HaskellAst (HsImplicitBndrs (GhcPass p) a) r where
    store = store . hsib_body

instance IsGhcPass p r => HaskellAst (HsValBinds (GhcPass p)) r where
    store (ValBinds _ binds sigs) = store sigs >> store binds
    store (XValBindsLR (NValBinds binds sigs)) = store sigs >> store (map snd binds)

instance IsGhcPass p r => HaskellAst (Sig (GhcPass p)) r where
    store (TypeSig _ ids types) = storeDefCtx DefSignature $ defining (store ids) >> store types
    store (PatSynSig _ ids types) = storeDefCtx DefPatternSynonym $ defining (store ids) >> store types
    store (ClassOpSig _ _ ids types) = storeDefCtx DefClassOpSignature $ defining (store ids) >> store types
    store (IdSig {}) = return () -- generated code
    store (FixSig _ sig) = store sig
    store (InlineSig _ ids _) = store ids
    store (SpecSig _ ids types _) = store ids >> store types
    store (SpecInstSig _ _ typ) = store typ
    store (MinimalSig _ _ formula) = store formula
    store (SCCFunSig _ _ ids _) = store ids
    store (CompleteMatchSig _ _ ids maybeIds) = store ids >> mapM_ store maybeIds
    store (XSig {}) = return ()

instance IsGhcPass p r => HaskellAst (HsType (GhcPass p)) r where
  store t
    | Just ls <- splitHsFunTys t
    = mapM_ (storeLoc $ storeDefCtx DefParameter . store) ls
  store (HsFunTy {}) = error "Should have been handled earlier"
  store (HsForAllTy _ vars body) = store vars >> store body
  store (HsQualTy _ ctx body) = store ctx >> store body
  store (HsTyVar _ _ id) = store id
  store (HsAppTy _ lhs rhs) = store lhs >> store rhs
  store (HsListTy _ t) = store t
  store (HsTupleTy _ _ []) = store (getTypedName unitTyCon) -- unit type
  store (HsTupleTy _ _ ts) = store ts
  store (HsSumTy _ ts) = store ts
  store (HsOpTy _ lhs op rhs) = store lhs >> store op >> store rhs
  store (HsParTy _ t) = store t
  store (HsIParamTy _ _ ty) = store ty -- implicit params are not resolved here
  store (HsStarTy _ _) = return ()
  store (HsKindSig _ t k) = store t >> store k
  store (HsSpliceTy _ splice) = store splice
  store (HsDocTy _ ty _) = store ty
  store (HsBangTy _ _ t) = store t
  store (HsRecTy _ fields) = store fields
  store (HsExplicitListTy _ _ ts) = store ts
  store (HsExplicitTupleTy _ ts) = store ts
  store (HsTyLit {}) = return () -- nothing to store
  store (HsWildCardTy {}) = return () -- nothing to store
  store (XHsType {}) = return ()

splitHsFunTys :: HsType p -> Maybe [LHsType p]
splitHsFunTys (HsFunTy _ t1 t2) = Just $ t1 : splitHsFunTys' t2
splitHsFunTys _ = Nothing

splitHsFunTys' :: LHsType p -> [LHsType p]
splitHsFunTys' (unLoc -> HsFunTy _ t1 t2) = t1 : splitHsFunTys' t2
splitHsFunTys' t = [t]

instance IsGhcPass p r => HaskellAst (HsTyVarBndr (GhcPass p)) r where
    store (UserTyVar _ id) = defining $ store id
    store (KindedTyVar _ id kind) = do
        defining $ store id
        store kind -- kinds are just types
    store (XTyVarBndr _) = return ()

instance IsGhcPass p r => HaskellAst (HsBindLR (GhcPass p) (GhcPass p)) r where
    store (FunBind _ funId matches _ _) = storeDefCtx DefValue $ defining (store funId) >> store matches
    store (PatBind _ lhs rhs _) = storeDefCtx DefValue $ store lhs >> store rhs
    store (VarBind _ id rhs _) = storeDefCtx DefValue $ do
      isInstanceDefinition <- inInstanceDefinition
      if isInstanceDefinition then store id else defining $ store id
      store rhs 
    store (AbsBinds _ _ _ _ _ binds _) = storeDefCtx DefValue $ store binds
    store (PatSynBind _ bind) = storeDefCtx DefPatternSynonym $ store bind
    store (XHsBindsLR {}) = return ()

instance (IsGhcPass p r, HaskellAst a r) => HaskellAst (MatchGroup (GhcPass p) a) r where 
    store (MG _ alts _) = store alts
    store (XMatchGroup {}) = return ()

instance (IsGhcPass p r, HaskellAst a r) => HaskellAst (Match (GhcPass p) a) r where 
    store (Match _ ctx pats grhss) = store ctx >> store pats >> store grhss
    store (XMatch {}) = return ()

instance (HaskellAst a r) => HaskellAst (HsMatchContext a) r where 
  store (FunRhs id _ _) = do
    isInstanceDefinition <- inInstanceDefinition
    if isInstanceDefinition then store id else defining $ store id
  store _ = return () -- all the other cases are unrelevant

instance (IsGhcPass p r, HaskellAst a r) => HaskellAst (GRHSs (GhcPass p) a) r where 
    store (GRHSs _ grhs localBinds) = store grhs >> store localBinds
    store (XGRHSs {}) = return ()

instance (IsGhcPass p r, HaskellAst a r) => HaskellAst (GRHS (GhcPass p) a) r where 
    store (GRHS _ guards body) = store guards >> store body
    store (XGRHS {}) = return ()

instance IsGhcPass p r => HaskellAst (HsLocalBinds (GhcPass p)) r where 
    store (HsValBinds _ binds) = storeDefCtx DefValue $ store binds
    store (HsIPBinds {}) = return () -- implicit parameters
    store (EmptyLocalBinds {}) = return ()
    store (XHsLocalBindsLR {}) = return ()

instance IsGhcPass p r => HaskellAst (HsExpr (GhcPass p)) r where 
    store (HsVar _ id) = store id
    store (HsUnboundVar {}) = return () -- hole or naming error
    store (HsConLikeOut _ conLike) = store (conLikeWrapId_maybe conLike)
    store (HsRecFld _ selector) = store selector
    store (HsOverLabel {}) = return () -- rebindable syntax
    store (HsIPVar {}) = return () -- implicit parameter
    store (HsOverLit {}) = return () -- literals
    store (HsLit {}) = return () -- literals
    store (HsLam _ mg) = store mg
    store (HsLamCase _ mg) = store mg
    store (HsApp _ e1 e2) = store e1 >> store e2
    store (HsAppType t e) = store t >> store e
    store (OpApp _ e1 e2 e3) = store e1 >> store e2 >> store e3
    store (NegApp _ e _) = store e
    store (HsPar _ e) = store e
    store (SectionL _ e1 e2) = store e1 >> store e2
    store (SectionR _ e1 e2) = store e1 >> store e2
    store (ExplicitTuple _ es _) = store es
    store (ExplicitSum _ _ _ e) = store e
    store (HsCase _ e mg) = store e >> store mg
    store (HsIf _ _ e1 e2 e3) = store e1 >> store e2 >> store e3
    store (HsMultiIf _ grhs) = store grhs
    store (HsLet _ locBinds e) = store locBinds >> store e
    store (HsDo _ _ stmts) = store stmts
    store (ExplicitList _ _ es) = store es
    store (RecordCon _ name flds) = store name >> store flds
    store (RecordUpd _ name flds) = store name >> store flds
    store (ExprWithTySig t e) = store e >> store t
    store (ArithSeq _ _ si) = store si
    store (HsSCC _ _ _ e) = store e
    store (HsCoreAnn _ _ _ e) = store e
    store (HsBracket _ br) = store br
    store (HsRnBracketOut _ br _) = store br
    store (HsTcBracketOut _ br _) = store br
    store (HsSpliceE _ sp) = store sp
    store (HsProc _ p c) = store p >> store c
    store (HsStatic _ e) = store e
    store (HsArrApp _ e1 e2 _ _) = store e1 >> store e2
    store (HsArrForm _ e _ c) = store e >> store c
    store (HsTick _ _ e) = store e
    store (HsBinTick _ _ _ e) = store e
    store (HsTickPragma _ _ _ _ e) = store e
    store (EWildPat {}) = return ()
    store (EAsPat _ at e) = defining (store at) >> store e
    store (EViewPat _ e1 e2) = store e1 >> store e2
    store (ELazyPat _ e) = store e
    store (HsWrap _ _ e) = store e
    store (XExpr {}) = return ()

instance IsGhcPass p r => HaskellAst (HsSplice (GhcPass p)) r where 
    store (HsTypedSplice _ _ _ e) = store e
    store (HsUntypedSplice _ _ _ e) = store e
    store (HsQuasiQuote _ n1 n2 _ _) = store n1 >> store n2
    store (HsSpliced _ _ spliced) = store spliced
    store (XSplice {}) = return ()

instance IsGhcPass p r => HaskellAst (HsSplicedThing (GhcPass p)) r where 
    store (HsSplicedExpr e) = store e
    store (HsSplicedTy t) = store t
    store (HsSplicedPat p) = store p

instance (IsGhcPass p r, HaskellAst a r) => HaskellAst (StmtLR (GhcPass p) (GhcPass p) a) r where 
    store (LastStmt _ e _ _) = store e
    store (BindStmt _ p e _ _) = store p >> store e
    store (ApplicativeStmt _ arg _) = store (map snd arg)
    store (BodyStmt _ e _ _) = store e
    store (LetStmt _ locBinds) = store locBinds
    store (ParStmt _ st e _) = store st >> store e
    store (TransStmt _ _ st bndrs using by _ _ fm)
        = store st >> store bndrs >> store using >> store by >> store fm
    store (RecStmt _ st n1 n2 _ _ _) = store st >> store n1 >> store n2
    store (XStmtLR {}) = return ()

instance IsGhcPass p r => HaskellAst (Pat (GhcPass p)) r where 
    store (WildPat {}) = return ()
    store (VarPat _ id) = defining $ store id
    store (LazyPat _ pat) = store pat
    store (AsPat _ as pat) = defining (store as) >> store pat
    store (ParPat _ pat) = store pat
    store (BangPat _ pat) = store pat
    store (ListPat _ pats) = store pats
    store (TuplePat _ pats _) = store pats
    store (SumPat _ pat _ _) = store pat
    store (ConPatIn n details) = store n >> store details
    store (ConPatOut {}) = return () -- after type checking
    store (ViewPat _ e pat) = store e >> store pat
    store (SplicePat _ sp) = store sp
    store (LitPat {}) = return ()
    store (NPat {}) = return ()
    store (NPlusKPat {}) = return ()
    store (SigPat ty pat) = store ty >> store pat
    store (CoPat _ _ p _) = store p
    store (XPat {}) = return ()

instance IsGhcPass p r => HaskellAst (HsTupArg (GhcPass p)) r where 
    store (Present _ e) = store e
    store (Missing {}) = return ()
    store (XTupArg {}) = return ()

instance (HaskellAst a r, HaskellAst b r) => HaskellAst (HsConDetails a b) r where 
    store (PrefixCon args) = store args
    store (RecCon r) = store r
    store (InfixCon a1 a2) = store a1 >> store a2

instance IsGhcPass p r => HaskellAst (PatSynBind (GhcPass p) (GhcPass p)) r where 
    store (PSB _ id args rhs _) = defining (store id) >> store args >> store rhs
    store (XPatSynBind {}) = return ()

instance HaskellAst a r => HaskellAst (RecordPatSynField a) r where 
    store (RecordPatSynField selector var) = defining (store selector) >> defining (store var)

instance IsGhcPass p r => HaskellAst (FixitySig (GhcPass p)) r where 
    store (FixitySig _ ids _) = store ids
    store (XFixitySig {}) = return ()

instance HaskellAst a r => HaskellAst (BooleanFormula a) r where
    store (Var a) = store a
    store (And formulas) = store formulas
    store (Or formulas) = store formulas
    store (Parens formula) = store formula

instance IsGhcPass p r => HaskellAst (ConDeclField (GhcPass p)) r where 
    store (ConDeclField _ names typ _) = defining (store names) >> store typ
    store (XConDeclField {}) = return ()

instance IsGhcPass p r => HaskellAst (FieldOcc (GhcPass p)) r where 
    store (FieldOcc selector _) = store selector
    store (XFieldOcc {}) = return ()

instance IsGhcPass p r => HaskellAst (AmbiguousFieldOcc (GhcPass p)) r where 
    store (Unambiguous name _) = store name
    store (Ambiguous {}) = return () -- will become unambiguous after type checking
    store (XAmbiguousFieldOcc {}) = return ()

instance (IsGhcPass p r, HaskellAst a r) => HaskellAst (HsRecFields (GhcPass p) a) r where 
    store (HsRecFields flds _) = store flds

instance (HaskellAst id r, HaskellAst arg r) => HaskellAst (HsRecField' id arg) r where 
    store (HsRecField lbl arg _) = store lbl >> store arg

instance IsGhcPass p r => HaskellAst (ArithSeqInfo (GhcPass p)) r where 
    store (From from) = store from
    store (FromThen from step) = store from >> store step
    store (FromTo from to) = store from >> store to
    store (FromThenTo from step to) = store from >> store step >> store to

instance IsGhcPass p r => HaskellAst (HsBracket (GhcPass p)) r where 
    store (ExpBr _ e) = store e
    store (PatBr _ p) = store p
    store (DecBrL _ d) = store d
    store (DecBrG _ d) = store d
    store (TypBr _ t) = store t
    store (VarBr _ _ v) = store v
    store (TExpBr _ e) = store e
    store (XBracket {}) = return ()

instance IsGhcPass p r => HaskellAst (HsGroup (GhcPass p)) r where 
    store (HsGroup _ vals spl tycl derivs fixs defds fords warns anns rules _)
        = store vals >> store spl >> store tycl >> store derivs >> store fixs >> store defds 
            >> store fords >> store warns >> store anns >> store rules
    store (XHsGroup {}) = return ()

instance IsGhcPass p r => HaskellAst (HsDecl (GhcPass p)) r where 
    store (TyClD _ d) = storeDefCtx DefTypeClass $ store d
    store (InstD _ d) = store d
    store (DerivD _ d) = store d
    store (ValD _ d) = store d
    store (SigD _ d) = store d
    store (DefD _ d) = store d
    store (ForD _ d) = store d
    store (WarningD _ d) = store d
    store (AnnD _ d) = store d
    store (RuleD _ d) = store d
    store (SpliceD _ d) = store d
    store (DocD {}) = return ()
    store (RoleAnnotD _ d) = store d
    store (XHsDecl {}) = return ()

instance IsGhcPass p r => HaskellAst (TyClDecl (GhcPass p)) r where 
    store (FamDecl _ d) = store d
    store (SynDecl _ name vars _ rhs) = defining (store name >> store vars) >> store rhs
    store (DataDecl _ name vars _ def) = defining (store name >> store vars) >> store def
    store (ClassDecl _ ctx name vars _ funDeps sigs methods assocTypes assocDefaults _)
        = store ctx >> defining (store name >> store vars) >> store funDeps >> store sigs
            >> store methods >> store assocTypes >> store assocDefaults
    store (XTyClDecl {}) = return ()

instance IsGhcPass p r => HaskellAst (InstDecl (GhcPass p)) r where 
    store (ClsInstD _ d) = storeDefCtx DefInstance $ store d
    store (DataFamInstD _ d) = store d
    store (TyFamInstD _ d) = store d
    store (XInstDecl {}) = return ()

instance IsGhcPass p r => HaskellAst (TyFamInstDecl (GhcPass p)) r where
    store (TyFamInstDecl eqn) = store eqn

instance (IsGhcPass p r, HaskellAst pat r, HaskellAst rhs r) => HaskellAst (FamEqn (GhcPass p) pat rhs) r where
    store (FamEqn _ name pats _ rhs) = defining (store name) >> store pats >> store rhs
    store (XFamEqn {}) = return ()

instance IsGhcPass p r => HaskellAst (DataFamInstDecl (GhcPass p)) r where
    store (DataFamInstDecl eqn) = store eqn

instance IsGhcPass p r => HaskellAst (HsDataDefn (GhcPass p)) r where
    store (HsDataDefn _ _ ctx _ ts cons derivs) = store ctx >> store ts >> store cons >> store derivs
    store (XHsDataDefn {}) = return ()

instance IsGhcPass p r => HaskellAst (HsDerivingClause (GhcPass p)) r where
    store (HsDerivingClause _ _ tys) = store tys
    store (XHsDerivingClause {}) = return ()

instance IsGhcPass p r => HaskellAst (ConDecl (GhcPass p)) r where
    store (ConDeclGADT _ names _ qvars ctx args res _) =
        defining (store names >> store qvars) >> store ctx >> store args >> store res
    store (ConDeclH98 _ name _ tvs ctx details _) =
        defining (store name >> store tvs) >> store ctx >> store details
    store (XConDecl {}) = return ()

instance IsGhcPass p r => HaskellAst (LHsQTyVars (GhcPass p)) r where
    store (HsQTvs _ tys) = store tys
    store (XLHsQTyVars {}) = return ()

instance IsGhcPass p r => HaskellAst (ClsInstDecl (GhcPass p)) r where
    store (ClsInstDecl _ ty binds sigs typeFamInst dataFamInst _)
        = store ty >> store binds >> store sigs >> store typeFamInst >> store dataFamInst
    store (XClsInstDecl {}) = return ()

instance IsGhcPass p r => HaskellAst (FamilyDecl (GhcPass p)) r where
    store (FamilyDecl _ info name tyVars _ sig inj)
        = store info >> defining (store name >> store tyVars) >> store sig >> store inj
    store (XFamilyDecl {}) = return ()

instance IsGhcPass p r => HaskellAst (InjectivityAnn (GhcPass p)) r where
    store (InjectivityAnn lhs rhs) = store lhs >> store rhs

instance IsGhcPass p r => HaskellAst (FamilyResultSig (GhcPass p)) r where
    store (NoSig {}) = return ()
    store (KindSig _ k) = store k
    store (TyVarSig _ tv) = store tv
    store (XFamilyResultSig {}) = return ()

instance IsGhcPass p r => HaskellAst (FamilyInfo (GhcPass p)) r where
    store DataFamily = return ()
    store OpenTypeFamily = return ()
    store (ClosedTypeFamily eqs) = store eqs

instance IsGhcPass p r => HaskellAst (RoleAnnotDecl (GhcPass p)) r where
    store (RoleAnnotDecl _ ids _) = store ids
    store (XRoleAnnotDecl {}) = return ()

instance IsGhcPass p r => HaskellAst (SpliceDecl (GhcPass p)) r where
    store (SpliceDecl _ spl _) = store spl
    store (XSpliceDecl {}) = return ()

instance IsGhcPass p r => HaskellAst (RuleDecls (GhcPass p)) r where
    store (HsRules _ _ rules) = store rules
    store (XRuleDecls {}) = return ()

instance IsGhcPass p r => HaskellAst (RuleDecl (GhcPass p)) r where
    store (HsRule _ _ _ bndrs e1 e2) = store bndrs >> store e1 >> store e2
    store (XRuleDecl {}) = return ()

instance IsGhcPass p r => HaskellAst (RuleBndr (GhcPass p)) r where
    store (RuleBndr _ name) = store name
    store (RuleBndrSig _ name ty) = store name >> store ty
    store (XRuleBndr {}) = return ()

instance IsGhcPass p r => HaskellAst (AnnDecl (GhcPass p)) r where
    store (HsAnnotation _ _ prov e) = store prov >> store e
    store (XAnnDecl {}) = return ()

instance HaskellAst n r => HaskellAst (AnnProvenance n) r where
    store (ValueAnnProvenance n) = store n
    store (TypeAnnProvenance n) = store n
    store (ModuleAnnProvenance) = return ()

instance IsGhcPass p r => HaskellAst (WarnDecls (GhcPass p)) r where
    store (Warnings _ _ w) = store w
    store (XWarnDecls {}) = return ()

instance IsGhcPass p r => HaskellAst (WarnDecl (GhcPass p)) r where
    store (Warning _ n _) = store n
    store (XWarnDecl {}) = return ()

instance IsGhcPass p r => HaskellAst (ForeignDecl (GhcPass p)) r where
    store (ForeignImport _ n t _) = defining (store n) >> store t
    store (ForeignExport _ n t _) = defining (store n) >> store t
    store (XForeignDecl {}) = return ()

instance IsGhcPass p r => HaskellAst (DefaultDecl (GhcPass p)) r where
    store (DefaultDecl _ ts) = store ts
    store (XDefaultDecl {}) = return ()

instance IsGhcPass p r => HaskellAst (DerivDecl (GhcPass p)) r where
    store (DerivDecl _ t _ _) = store t
    store (XDerivDecl {}) = return ()

instance IsGhcPass p r => HaskellAst (HsCmdTop (GhcPass p)) r where
    store (HsCmdTop _ c) = store c
    store (XCmdTop {}) = return ()

instance IsGhcPass p r => HaskellAst (HsCmd (GhcPass p)) r where
    store (HsCmdArrApp _ e1 e2 _ _) = store e1 >> store e2
    store (HsCmdArrForm _ e _ _ c) = store e >> store c
    store (HsCmdApp _ c e) = store c >> store e
    store (HsCmdLam _ mg) = store mg
    store (HsCmdPar _ c) = store c
    store (HsCmdCase _ e mg) = store e >> store mg
    store (HsCmdIf _ _ e c1 c2) = store e >> store c1 >> store c2
    store (HsCmdLet _ bnds c) = store bnds >> store c
    store (HsCmdDo _ stmt) = store stmt
    store (HsCmdWrap _ _ c) = store c
    store (XCmd {}) = return ()

instance (IsGhcPass p1 r, IsGhcPass p2 r) => HaskellAst (ParStmtBlock (GhcPass p1) (GhcPass p2)) r where
    store (ParStmtBlock _ e n _) = store e >> store n
    store (XParStmtBlock {}) = return ()

instance IsGhcPass p r => HaskellAst (ApplicativeArg (GhcPass p)) r where
    store (ApplicativeArgOne _ p e _) = store p >> store e
    store (ApplicativeArgMany _ stmts e p) = store stmts >> store e >> store p
    store (XApplicativeArg {}) = return ()

instance IsGhcPass p r => HaskellAst (TyClGroup (GhcPass p)) r where
    store (TyClGroup _ tyclds roles insts) = store tyclds >> store roles >> store insts
    store (XTyClGroup {}) = return ()

instance IsGhcPass p r => HaskellAst (HsModule (GhcPass p)) r where
  store (HsModule _nm _exports _imports decls _ _) = store decls

instance HaskellAst TyThing TypeRecord where
  store (AnId id) = store id
  store (AConLike conLike) = store conLike
  store (ATyCon tycon) = store tycon
  store (ACoAxiom _) = return ()

instance HaskellAst ConLike TypeRecord where
  store (RealDataCon dataCon) = store dataCon
  store (PatSynCon patSyn) = store patSyn

instance HaskellAst TyCon TypeRecord where
  store tc = do 
    currentModuleName <- asks scModuleName
    tell [ nameTypeToTypeRecord currentModuleName (tyConName tc) (tyConKind tc) ]
    store (tyConDataCons_maybe tc)

instance HaskellAst PatSyn TypeRecord where
  store _ps = return ()

instance HaskellAst DataCon TypeRecord where
  store dc = do
    currentModuleName <- asks scModuleName
    tell [ nameTypeToTypeRecord currentModuleName (dataConName dc) (dataConRepType dc) ]
