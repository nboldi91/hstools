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

module Language.Haskell.HsTools.PersistNameInfo where
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.Map as M

import HsDecls
import HsExtension
import HsBinds
import HsTypes
import Outputable
import SrcLoc
import Bag
import HsExpr
import Name
import Module
import Unique
import HsPat
import BooleanFormula
import TcRnTypes
import Var
import TyCoRep

import Database.PostgreSQL.Simple

data NameRecord = NameRecord
  { nmName :: String
  , nmIsDefined :: Bool
  , nmPos :: NodePos
  }

data NodePos = NodePos
  { npStartRow :: Int
  , npStartCol :: Int
  , npEndRow :: Int
  , npEndCol :: Int
  } deriving (Eq, Ord, Show)

srcSpanToNodePos :: SrcSpan -> Maybe NodePos
srcSpanToNodePos (RealSrcSpan span) = Just $ realSrcSpanToNodePos span
srcSpanToNodePos _ = Nothing

realSrcSpanToNodePos :: RealSrcSpan -> NodePos
realSrcSpanToNodePos span
  = (NodePos (srcLocLine start) (srcLocCol start) (srcLocLine end) (srcLocCol end))
  where
    start = realSrcSpanStart span
    end = realSrcSpanEnd span

data TypeRecord = TypeRecord
  { trAstNode :: Int
  , trType :: String
  } deriving Show

-- TODO: look up by the name first and then by the location

storeTypes :: Connection -> (String, Int) -> TcGblEnv -> IO ()
storeTypes conn (moduleName, moduleId) env = do
    astNodes <- query conn "SELECT startRow, startColumn, endRow, endColumn, astId FROM ast WHERE module = ?" (Only moduleId)
    let astNodeMap = M.fromList $ map (\(startRow, startColumn, endRow, endColumn, astId) 
                                          -> (NodePos startRow startColumn endRow endColumn, astId)) 
                                      astNodes
    ((), types) <- runWriterT (runReaderT storeEnv (StoreContext moduleName noSrcSpan False astNodeMap))
    persistTypes conn types
  where
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

persistTypes :: Connection -> [TypeRecord] -> IO ()
persistTypes conn types = void $ executeMany conn
    "INSERT INTO types (astNode, type) VALUES (?, ?)"
    (map convertType types)
  where convertType (TypeRecord astNode typ) = (astNode, typ)

storeNames :: Connection -> (String, Int) -> HsGroup GhcRn -> IO ()
storeNames conn (moduleName, moduleId) gr = do
    ((), names) <- runWriterT (runReaderT (store gr) (StoreContext moduleName noSrcSpan False M.empty))
    persistNames conn moduleId names

persistNames :: Connection -> Int -> [NameRecord] -> IO ()
persistNames conn moduleId names = do
    astIds <- returning conn
      "INSERT INTO ast (module, startRow, startColumn, endRow, endColumn) VALUES (?, ?, ?, ?, ?) RETURNING astId"
      (map convertLocation names)
    void $ executeMany conn
      "INSERT INTO names (astNode, name, isDefined) VALUES (?, ?, ?)"
      (map convertName (names `zip` concat astIds))
  where
    convertName ((NameRecord name isDefined _), id) = (id :: Int, name, isDefined)
    convertLocation (NameRecord _ _ (NodePos startRow startColumn endRow endColumn))
      = (moduleId, startRow, startColumn, endRow, endColumn)

type StoreM r = ReaderT StoreContext (WriterT [r] IO)

data StoreContext = StoreContext
  { scModuleName :: String
  , scSpan :: SrcSpan
  , scDefining :: Bool
  , scNodeMap :: M.Map NodePos Int
  }

type IsGhcPass p r =
  ( HaskellAst Name r -- skip
  , HaskellAst (IdP (GhcPass p)) r
  , HaskellAst (XExprWithTySig (GhcPass p)) r
  , HaskellAst (XSigPat (GhcPass p)) r
  , HaskellAst (XCFieldOcc (GhcPass p)) r
  , HaskellAst (XUnambiguous (GhcPass p)) r
  , HaskellAst (XAppTypeE (GhcPass p)) r
  , HaskellAst (XCFieldOcc (GhcPass p)) r
  )

defining :: StoreM r a -> StoreM r a
defining = local (\l -> l { scDefining = True })

withSpan :: SrcSpan -> StoreM r a -> StoreM r a
withSpan span = local (\l -> l { scSpan = span })

class HaskellAst a r where
    store :: a -> StoreM r ()

instance HaskellAst Name NameRecord where
    store id = asks scSpan >>= \case
        RealSrcSpan span -> do
            currentModuleName <- asks scModuleName
            defined <- asks scDefining
            let modName = nameModule_maybe id
                fullName = case modName of
                            Just mn -> showSDocUnsafe (pprModule mn) ++ "." ++ showSDocUnsafe (ppr id)
                            Nothing -> currentModuleName ++ "." ++ showSDocUnsafe (ppr id) ++ ":" ++ showSDocUnsafe (pprUniqueAlways (getUnique id)) 
                storedName = NameRecord
                    { nmName = fullName
                    , nmIsDefined = defined
                    , nmPos = realSrcSpanToNodePos span
                    }
            tell [storedName] 
        _ -> liftIO $ putStrLn $ "WARNING " ++ (showSDocUnsafe $ ppr id) ++ " does not have a real src span"

instance HaskellAst Name TypeRecord where
  store _ = return ()

instance HaskellAst Id TypeRecord where
  store id = do 
    span <- asks scSpan
    case srcSpanToNodePos span of
      Just np -> do
        node <- asks (M.lookup np . scNodeMap)
        let typeStr = showSDocUnsafe $ ppr $ varType id
        case node of
          Just astNode -> tell [ TypeRecord astNode typeStr ]
          Nothing -> return () -- we don't have this location in the AST
      Nothing -> return () -- no location for the original id

instance HaskellAst Type r where
  store _ = return ()

instance HaskellAst a r => HaskellAst (Located a) r where
  store (L span ast)
    = (if isGoodSrcSpan span then withSpan span else id) $ store ast

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
    store (TypeSig _ ids types) = defining (store ids) >> store types
    store (PatSynSig _ ids types) = defining (store ids) >> store types
    store (ClassOpSig _ _ ids types) = defining (store ids) >> store types
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
    store (HsForAllTy _ vars body) = store vars >> store body
    store (HsQualTy _ ctx body) = store ctx >> store body
    store (HsTyVar _ _ id) = store id
    store (HsAppTy _ lhs rhs) = store lhs >> store rhs
    store (HsFunTy _ lhs rhs) = store lhs >> store rhs
    store (HsListTy _ t) = store t
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

instance IsGhcPass p r => HaskellAst (HsTyVarBndr (GhcPass p)) r where
    store (UserTyVar _ id) = defining $ store id
    store (KindedTyVar _ id kind) = do
        defining $ store id
        store kind -- kinds are just types
    store (XTyVarBndr _) = return ()

instance IsGhcPass p r => HaskellAst (HsBindLR (GhcPass p) (GhcPass p)) r where
    store (FunBind _ id matches _ _) = do
        defining $ store id
        store matches
    store (PatBind _ lhs rhs _) = store lhs >> store rhs
    store (VarBind _ id rhs _) = store id >> store rhs 
    store (AbsBinds _ _ _ _ _ binds _) = store binds
    store (PatSynBind _ bind) = store bind
    store (XHsBindsLR {}) = return ()

instance (IsGhcPass p r, HaskellAst a r) => HaskellAst (MatchGroup (GhcPass p) a) r where 
    store (MG _ alts _) = store alts
    store (XMatchGroup {}) = return ()

instance (IsGhcPass p r, HaskellAst a r) => HaskellAst (Match (GhcPass p) a) r where 
    store (Match _ _ pats grhss) = store pats >> store grhss
    store (XMatch {}) = return ()

instance (IsGhcPass p r, HaskellAst a r) => HaskellAst (GRHSs (GhcPass p) a) r where 
    store (GRHSs _ grhs localBinds) = store grhs >> store localBinds
    store (XGRHSs {}) = return ()

instance (IsGhcPass p r, HaskellAst a r) => HaskellAst (GRHS (GhcPass p) a) r where 
    store (GRHS _ guards body) = store guards >> store body
    store (XGRHS {}) = return ()

instance IsGhcPass p r => HaskellAst (HsLocalBinds (GhcPass p)) r where 
    store (HsValBinds _ binds) = store binds
    store (HsIPBinds {}) = return () -- implicit parameters
    store (EmptyLocalBinds {}) = return ()
    store (XHsLocalBindsLR {}) = return ()

instance IsGhcPass p r => HaskellAst (HsExpr (GhcPass p)) r where 
    store (HsVar _ id) = store id
    store (HsUnboundVar {}) = return () -- hole or naming error
    store (HsConLikeOut {}) = return () -- only after type checking
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
    store (HsTcBracketOut {}) = return ()
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
    store (TyClD _ d) = store d
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
    store (ClsInstD _ d) = store d
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
