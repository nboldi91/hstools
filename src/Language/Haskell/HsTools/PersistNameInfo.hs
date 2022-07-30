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

module Language.Haskell.HsTools.PersistNameInfo where
import Control.Monad.Writer
import Control.Monad.Reader

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

import Database.PostgreSQL.Simple

data NameRecord = NameRecord
    { nmName :: String
    , nmIsDefined :: Bool
    , nmStartRow :: Int
    , nmEndRow :: Int
    , nmStartCol :: Int
    , nmEndCol :: Int
    }

storeNames :: Connection -> (String, Int) -> HsGroup GhcRn -> IO ()
storeNames conn (moduleName, moduleId) gr = do
    ((), names) <- runWriterT (runReaderT (store gr) (StoreContext moduleName noSrcSpan False))
    persistNames conn moduleId names

persistNames :: Connection -> Int -> [NameRecord] -> IO ()
persistNames conn moduleId names = void $ executeMany conn
    "INSERT INTO names (module, name, isDefined, startRow, startColumn, endRow, endColumn) VALUES (?, ?, ?, ?, ?, ?, ?)"
    (map convertName names)
  where convertName (NameRecord name isDefined startRow startCol endRow endCol)
            = (moduleId, name, isDefined, startRow, startCol, endRow, endCol)

type StoreM = ReaderT StoreContext (WriterT [NameRecord] IO)

data StoreContext = StoreContext
  { scModuleName :: String
  , scSpan :: SrcSpan
  , scDefining :: Bool
  }

type IsGhcPass p =
     ( HaskellAst (IdP (GhcPass p))
     , HaskellAst (XExprWithTySig (GhcPass p))
     , HaskellAst (XSigPat (GhcPass p))
     , HaskellAst (XCFieldOcc (GhcPass p))
     , HaskellAst (XUnambiguous (GhcPass p))
     , HaskellAst (XAppTypeE (GhcPass p))
     , HaskellAst (XCFieldOcc (GhcPass p))
     )

defining :: StoreM a -> StoreM a
defining = local (\l -> l { scDefining = True })

class HaskellAst a where
    store :: a -> StoreM ()

instance HaskellAst Name where
    store id = asks scSpan >>= \sp -> case sp of
        RealSrcSpan span -> do
            currentModuleName <- asks scModuleName
            defined <- asks scDefining
            let start = realSrcSpanStart span
                end = realSrcSpanEnd span
                modName = nameModule_maybe id
                fullName = case modName of
                            Just mn -> showSDocUnsafe (pprModule mn) ++ "." ++ showSDocUnsafe (ppr id)
                            Nothing -> currentModuleName ++ "." ++ showSDocUnsafe (ppr id) ++ ":" ++ showSDocUnsafe (pprUniqueAlways (getUnique id)) 
                storedName = NameRecord
                    { nmName = fullName
                    , nmIsDefined = defined
                    , nmStartRow = srcLocLine start
                    , nmEndRow = srcLocCol start
                    , nmStartCol = srcLocLine end
                    , nmEndCol = srcLocCol end
                    }
            tell [storedName] 
        _ -> liftIO $ putStrLn $ "WARNING " ++ (showSDocUnsafe $ ppr id) ++ " does not have a real src span"


instance HaskellAst a => HaskellAst (Located a) where
    store (L span ast) = local (\l -> l { scSpan = span }) $ store ast 

instance HaskellAst a => HaskellAst [a] where
    store = mapM_ store

instance HaskellAst a => HaskellAst (Bag a) where
    store = mapBagM_ store

instance HaskellAst a => HaskellAst (Maybe a) where
    store (Just a) = store a
    store Nothing = return ()

instance (HaskellAst a, HaskellAst b) => HaskellAst (a, b) where
    store (a, b) = store a >> store b

instance HaskellAst a => HaskellAst (HsWildCardBndrs (GhcPass p) a) where
    store = store . hswc_body

instance HaskellAst a => HaskellAst (HsImplicitBndrs (GhcPass p) a) where
    store = store . hsib_body

instance IsGhcPass p => HaskellAst (HsValBinds (GhcPass p)) where
    store (ValBinds _ binds sigs) = store sigs >> store binds
    store (XValBindsLR (NValBinds binds sigs)) = store sigs >> store (map snd binds)

instance IsGhcPass p => HaskellAst (Sig (GhcPass p)) where
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

instance IsGhcPass p => HaskellAst (HsType (GhcPass p)) where
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

instance IsGhcPass p => HaskellAst (HsTyVarBndr (GhcPass p)) where
    store (UserTyVar _ id) = defining $ store id
    store (KindedTyVar _ id kind) = do
        defining $ store id
        store kind -- kinds are just types
    store (XTyVarBndr _) = return ()

instance IsGhcPass p => HaskellAst (HsBindLR (GhcPass p) (GhcPass p)) where
    store (FunBind _ id matches _ _) = do
        defining $ store id
        store matches
    store (PatBind _ lhs rhs _) = store lhs >> store rhs
    store (VarBind {}) = return () -- introduced by type checker
    store (AbsBinds {}) = return () -- introduced by type checker
    store (PatSynBind _ bind) = store bind
    store (XHsBindsLR {}) = return ()

instance (IsGhcPass p, HaskellAst a) => HaskellAst (MatchGroup (GhcPass p) a) where 
    store (MG _ alts _) = store alts
    store (XMatchGroup {}) = return ()

instance (IsGhcPass p, HaskellAst a) => HaskellAst (Match (GhcPass p) a) where 
    store (Match _ _ pats grhss) = store pats >> store grhss
    store (XMatch {}) = return ()

instance (IsGhcPass p, HaskellAst a) => HaskellAst (GRHSs (GhcPass p) a) where 
    store (GRHSs _ grhs localBinds) = store grhs >> store localBinds
    store (XGRHSs {}) = return ()

instance (IsGhcPass p, HaskellAst a) => HaskellAst (GRHS (GhcPass p) a) where 
    store (GRHS _ guards body) = store guards >> store body
    store (XGRHS {}) = return ()

instance IsGhcPass p => HaskellAst (HsLocalBinds (GhcPass p)) where 
    store (HsValBinds _ binds) = store binds
    store (HsIPBinds {}) = return () -- implicit parameters
    store (EmptyLocalBinds {}) = return ()
    store (XHsLocalBindsLR {}) = return ()

instance IsGhcPass p => HaskellAst (HsExpr (GhcPass p)) where 
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

instance IsGhcPass p => HaskellAst (HsSplice (GhcPass p)) where 
    store (HsTypedSplice _ _ _ e) = store e
    store (HsUntypedSplice _ _ _ e) = store e
    store (HsQuasiQuote _ n1 n2 _ _) = store n1 >> store n2
    store (HsSpliced _ _ spliced) = store spliced
    store (XSplice {}) = return ()

instance IsGhcPass p => HaskellAst (HsSplicedThing (GhcPass p)) where 
    store (HsSplicedExpr e) = store e
    store (HsSplicedTy t) = store t
    store (HsSplicedPat p) = store p

instance (IsGhcPass p, HaskellAst a) => HaskellAst (StmtLR (GhcPass p) (GhcPass p) a) where 
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

instance IsGhcPass p => HaskellAst (Pat (GhcPass p)) where 
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

instance IsGhcPass p => HaskellAst (HsTupArg (GhcPass p)) where 
    store (Present _ e) = store e
    store (Missing {}) = return ()
    store (XTupArg {}) = return ()

instance (HaskellAst a, HaskellAst r) => HaskellAst (HsConDetails a r) where 
    store (PrefixCon args) = store args
    store (RecCon r) = store r
    store (InfixCon a1 a2) = store a1 >> store a2

instance IsGhcPass p => HaskellAst (PatSynBind (GhcPass p) (GhcPass p)) where 
    store (PSB _ id args rhs _) = defining (store id) >> store args >> store rhs
    store (XPatSynBind {}) = return ()

instance HaskellAst a => HaskellAst (RecordPatSynField a) where 
    store (RecordPatSynField selector var) = defining (store selector) >> defining (store var)

instance IsGhcPass p => HaskellAst (FixitySig (GhcPass p)) where 
    store (FixitySig _ ids _) = store ids
    store (XFixitySig {}) = return ()

instance HaskellAst a => HaskellAst (BooleanFormula a) where
    store (Var a) = store a
    store (And formulas) = store formulas
    store (Or formulas) = store formulas
    store (Parens formula) = store formula

instance IsGhcPass p => HaskellAst (ConDeclField (GhcPass p)) where 
    store (ConDeclField _ names typ _) = defining (store names) >> store typ
    store (XConDeclField {}) = return ()

instance IsGhcPass p => HaskellAst (FieldOcc (GhcPass p)) where 
    store (FieldOcc selector _) = store selector
    store (XFieldOcc {}) = return ()

instance IsGhcPass p => HaskellAst (AmbiguousFieldOcc (GhcPass p)) where 
    store (Unambiguous name _) = store name
    store (Ambiguous {}) = return () -- will become unambiguous after type checking
    store (XAmbiguousFieldOcc {}) = return ()

instance (IsGhcPass p, HaskellAst a) => HaskellAst (HsRecFields (GhcPass p) a) where 
    store (HsRecFields flds _) = store flds

instance (HaskellAst id, HaskellAst arg) => HaskellAst (HsRecField' id arg) where 
    store (HsRecField lbl arg _) = store lbl >> store arg

instance IsGhcPass p => HaskellAst (ArithSeqInfo (GhcPass p)) where 
    store (From from) = store from
    store (FromThen from step) = store from >> store step
    store (FromTo from to) = store from >> store to
    store (FromThenTo from step to) = store from >> store step >> store to

instance IsGhcPass p => HaskellAst (HsBracket (GhcPass p)) where 
    store (ExpBr _ e) = store e
    store (PatBr _ p) = store p
    store (DecBrL _ d) = store d
    store (DecBrG _ d) = store d
    store (TypBr _ t) = store t
    store (VarBr _ _ v) = store v
    store (TExpBr _ e) = store e
    store (XBracket {}) = return ()

instance IsGhcPass p => HaskellAst (HsGroup (GhcPass p)) where 
    store (HsGroup _ vals spl tycl derivs fixs defds fords warns anns rules _)
        = store vals >> store spl >> store tycl >> store derivs >> store fixs >> store defds 
            >> store fords >> store warns >> store anns >> store rules
    store (XHsGroup {}) = return ()

instance IsGhcPass p => HaskellAst (HsDecl (GhcPass p)) where 
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

instance IsGhcPass p => HaskellAst (TyClDecl (GhcPass p)) where 
    store (FamDecl _ d) = store d
    store (SynDecl _ name vars _ rhs) = defining (store name >> store vars) >> store rhs
    store (DataDecl _ name vars _ def) = defining (store name >> store vars) >> store def
    store (ClassDecl _ ctx name vars _ funDeps sigs methods assocTypes assocDefaults _)
        = store ctx >> defining (store name >> store vars) >> store funDeps >> store sigs
            >> store methods >> store assocTypes >> store assocDefaults
    store (XTyClDecl {}) = return ()

instance IsGhcPass p => HaskellAst (InstDecl (GhcPass p)) where 
    store (ClsInstD _ d) = store d
    store (DataFamInstD _ d) = store d
    store (TyFamInstD _ d) = store d
    store (XInstDecl {}) = return ()

instance IsGhcPass p => HaskellAst (TyFamInstDecl (GhcPass p)) where
    store (TyFamInstDecl eqn) = store eqn

instance (IsGhcPass p, HaskellAst pat, HaskellAst rhs) => HaskellAst (FamEqn (GhcPass p) pat rhs) where
    store (FamEqn _ name pats _ rhs) = defining (store name) >> store pats >> store rhs
    store (XFamEqn {}) = return ()

instance IsGhcPass p => HaskellAst (DataFamInstDecl (GhcPass p)) where
    store (DataFamInstDecl eqn) = store eqn

instance IsGhcPass p => HaskellAst (HsDataDefn (GhcPass p)) where
    store (HsDataDefn _ _ ctx _ ts cons derivs) = store ctx >> store ts >> store cons >> store derivs
    store (XHsDataDefn {}) = return ()

instance IsGhcPass p => HaskellAst (HsDerivingClause (GhcPass p)) where
    store (HsDerivingClause _ _ tys) = store tys
    store (XHsDerivingClause {}) = return ()

instance IsGhcPass p => HaskellAst (ConDecl (GhcPass p)) where
    store (ConDeclGADT _ names _ qvars ctx args res _) =
        defining (store names >> store qvars) >> store ctx >> store args >> store res
    store (ConDeclH98 _ name _ tvs ctx details _) =
        defining (store name >> store tvs) >> store ctx >> store details
    store (XConDecl {}) = return ()

instance IsGhcPass p => HaskellAst (LHsQTyVars (GhcPass p)) where
    store (HsQTvs _ tys) = store tys
    store (XLHsQTyVars {}) = return ()

instance IsGhcPass p => HaskellAst (ClsInstDecl (GhcPass p)) where
    store (ClsInstDecl _ ty binds sigs typeFamInst dataFamInst _)
        = store ty >> store binds >> store sigs >> store typeFamInst >> store dataFamInst
    store (XClsInstDecl {}) = return ()

instance IsGhcPass p => HaskellAst (FamilyDecl (GhcPass p)) where
    store (FamilyDecl _ info name tyVars _ sig inj)
        = store info >> defining (store name >> store tyVars) >> store sig >> store inj
    store (XFamilyDecl {}) = return ()

instance IsGhcPass p => HaskellAst (InjectivityAnn (GhcPass p)) where
    store (InjectivityAnn lhs rhs) = store lhs >> store rhs

instance IsGhcPass p => HaskellAst (FamilyResultSig (GhcPass p)) where
    store (NoSig {}) = return ()
    store (KindSig _ k) = store k
    store (TyVarSig _ tv) = store tv
    store (XFamilyResultSig {}) = return ()

instance IsGhcPass p => HaskellAst (FamilyInfo (GhcPass p)) where
    store DataFamily = return ()
    store OpenTypeFamily = return ()
    store (ClosedTypeFamily eqs) = store eqs

instance IsGhcPass p => HaskellAst (RoleAnnotDecl (GhcPass p)) where
    store (RoleAnnotDecl _ ids _) = store ids
    store (XRoleAnnotDecl {}) = return ()

instance IsGhcPass p => HaskellAst (SpliceDecl (GhcPass p)) where
    store (SpliceDecl _ spl _) = store spl
    store (XSpliceDecl {}) = return ()

instance IsGhcPass p => HaskellAst (RuleDecls (GhcPass p)) where
    store (HsRules _ _ rules) = store rules
    store (XRuleDecls {}) = return ()

instance IsGhcPass p => HaskellAst (RuleDecl (GhcPass p)) where
    store (HsRule _ _ _ bndrs e1 e2) = store bndrs >> store e1 >> store e2
    store (XRuleDecl {}) = return ()

instance IsGhcPass p => HaskellAst (RuleBndr (GhcPass p)) where
    store (RuleBndr _ name) = store name
    store (RuleBndrSig _ name ty) = store name >> store ty
    store (XRuleBndr {}) = return ()

instance IsGhcPass p => HaskellAst (AnnDecl (GhcPass p)) where
    store (HsAnnotation _ _ prov e) = store prov >> store e
    store (XAnnDecl {}) = return ()

instance HaskellAst n => HaskellAst (AnnProvenance n) where
    store (ValueAnnProvenance n) = store n
    store (TypeAnnProvenance n) = store n
    store (ModuleAnnProvenance) = return ()

instance IsGhcPass p => HaskellAst (WarnDecls (GhcPass p)) where
    store (Warnings _ _ w) = store w
    store (XWarnDecls {}) = return ()

instance IsGhcPass p => HaskellAst (WarnDecl (GhcPass p)) where
    store (Warning _ n _) = store n
    store (XWarnDecl {}) = return ()

instance IsGhcPass p => HaskellAst (ForeignDecl (GhcPass p)) where
    store (ForeignImport _ n t _) = defining (store n) >> store t
    store (ForeignExport _ n t _) = defining (store n) >> store t
    store (XForeignDecl {}) = return ()

instance IsGhcPass p => HaskellAst (DefaultDecl (GhcPass p)) where
    store (DefaultDecl _ ts) = store ts
    store (XDefaultDecl {}) = return ()

instance IsGhcPass p => HaskellAst (DerivDecl (GhcPass p)) where
    store (DerivDecl _ t _ _) = store t
    store (XDerivDecl {}) = return ()

instance IsGhcPass p => HaskellAst (HsCmdTop (GhcPass p)) where
    store (HsCmdTop _ c) = store c
    store (XCmdTop {}) = return ()

instance IsGhcPass p => HaskellAst (HsCmd (GhcPass p)) where
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

instance (IsGhcPass p1, IsGhcPass p2) => HaskellAst (ParStmtBlock (GhcPass p1) (GhcPass p2)) where
    store (ParStmtBlock _ e n _) = store e >> store n
    store (XParStmtBlock {}) = return ()

instance IsGhcPass p => HaskellAst (ApplicativeArg (GhcPass p)) where
    store (ApplicativeArgOne _ p e _) = store p >> store e
    store (ApplicativeArgMany _ stmts e p) = store stmts >> store e >> store p
    store (XApplicativeArg {}) = return ()

instance IsGhcPass p => HaskellAst (TyClGroup (GhcPass p)) where
    store (TyClGroup _ tyclds roles insts) = store tyclds >> store roles >> store insts
    store (XTyClGroup {}) = return ()







storeIdNames :: Bool -> Located (IdP GhcRn) -> StoreM ()
storeIdNames defined (L (RealSrcSpan span) id) = do
    currentModuleName <- asks scModuleName
    let start = realSrcSpanStart span
        end = realSrcSpanEnd span
        modName = nameModule_maybe id
        fullName = case modName of
                    Just mn -> showSDocUnsafe (pprModule mn) ++ "." ++ showSDocUnsafe (ppr id)
                    Nothing -> currentModuleName ++ "." ++ showSDocUnsafe (ppr id) ++ ":" ++ showSDocUnsafe (pprUniqueAlways (getUnique id)) 
        storedName = NameRecord
            { nmName = fullName
            , nmIsDefined = defined
            , nmStartRow = srcLocLine start
            , nmEndRow = srcLocCol start
            , nmStartCol = srcLocLine end
            , nmEndCol = srcLocCol end
            }
    tell [storedName] 
storeIdNames _ id = do
    liftIO $ putStrLn $ "WARNING " ++ (showSDocUnsafe $ ppr id) ++ " does not have a real src span"


