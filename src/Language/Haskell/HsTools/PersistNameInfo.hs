{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.HsTools.PersistNameInfo where
import Data.Data
import Control.Monad.Writer
import Control.Monad.Reader

import HscTypes
import HsDecls
import HsExtension
import HsBinds
import HsTypes
import TcRnTypes
import Outputable
import SrcLoc
import Bag
import HsExpr
import FastString
import Name
import Module
import Unique
import HsPat

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
    ((), names) <- runWriterT (runReaderT (storeValBindNames $ hs_valds gr) (StoreContext moduleName))
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
  }

storeValBindNames :: HsValBinds GhcRn -> StoreM ()
storeValBindNames (ValBinds _ binds sigs) = do 
    mapM_ storeSignatureNames (map unLoc sigs)
    storeValLhsBindsNames binds
storeValBindNames (XValBindsLR (NValBinds binds sigs)) = do
    mapM_ storeSignatureNames (map unLoc sigs)
    mapM_ storeValLhsBindsNames (map snd binds)

storeSignatureNames :: Sig GhcRn -> StoreM ()
storeSignatureNames (TypeSig _ ids types) = do
    mapM_ (storeIdNames True) ids
    storeTypeNames $ unLoc $ hsib_body $ hswc_body types
storeSignatureNames _ = return () -- TODO

storeTypeNames :: HsType GhcRn -> StoreM ()
storeTypeNames (HsForAllTy _ vars body) = do
    mapM_ (storeTyVarBndrNames . unLoc) vars
    storeTypeNames (unLoc body)
storeTypeNames (HsQualTy _ ctx body) = storeTypeNames (unLoc body)
storeTypeNames (HsTyVar _ _ id) = storeIdNames False id
storeTypeNames (HsAppTy _ lhs rhs) = storeTypeNames (unLoc lhs) >> storeTypeNames (unLoc rhs)
storeTypeNames (HsFunTy _ lhs rhs) = storeTypeNames (unLoc lhs) >> storeTypeNames (unLoc rhs)
storeTypeNames (HsListTy _ t) = storeTypeNames (unLoc t)
storeTypeNames (HsTupleTy _ _ ts) = mapM_ (storeTypeNames . unLoc) ts
storeTypeNames (HsSumTy _ ts) = mapM_ (storeTypeNames . unLoc) ts
storeTypeNames (HsOpTy _ lhs op rhs) = do 
    storeTypeNames (unLoc lhs)
    storeIdNames False op
    storeTypeNames (unLoc rhs)
storeTypeNames (HsParTy _ t) = storeTypeNames (unLoc t)
storeTypeNames (HsKindSig _ t k) = storeTypeNames (unLoc t) >> storeTypeNames (unLoc k)
storeTypeNames (HsBangTy _ _ t) = storeTypeNames (unLoc t)
-- storeTypeNames (HsRecTy {}) = error "Not supported: HsRecTy"
storeTypeNames (HsExplicitListTy _ _ ts) = mapM_ (storeTypeNames . unLoc) ts
storeTypeNames (HsExplicitTupleTy _ ts) = mapM_ (storeTypeNames . unLoc) ts
storeTypeNames _ = return ()

storeTyVarBndrNames :: HsTyVarBndr GhcRn -> StoreM ()
storeTyVarBndrNames (UserTyVar _ id) = storeIdNames True id
storeTyVarBndrNames (KindedTyVar _ id kind) = do
    storeIdNames True id
    storeTypeNames (unLoc kind) -- kinds are just types
storeTyVarBndrNames (XTyVarBndr _) = return ()

storeValLhsBindsNames :: LHsBinds GhcRn -> StoreM ()
storeValLhsBindsNames binds = mapM_ (storeValLhsBindNames . unLoc) binds

storeValLhsBindNames :: HsBindLR GhcRn GhcRn -> StoreM ()
storeValLhsBindNames (FunBind _ id matches _ _) = do
    storeIdNames True id
    storeMatchesNames matches
storeValLhsBindNames _ = return () -- TODO

storeMatchesNames :: MatchGroup GhcRn (LHsExpr GhcRn) -> StoreM ()
storeMatchesNames (MG _ alts _) = mapM_ (storeMatchNames . unLoc) (unLoc alts)
storeMatchesNames _ = return () -- TODO

storeMatchNames :: Match GhcRn (LHsExpr GhcRn) -> StoreM ()
storeMatchNames (Match _ _ _pats grhss) = storeGRHSsNames grhss
storeMatchNames _ = return () -- TODO

storeGRHSsNames :: GRHSs GhcRn (LHsExpr GhcRn) -> StoreM ()
storeGRHSsNames (GRHSs _ grhs _localBinds) = mapM_ (storeGRHSNames . unLoc) grhs
storeGRHSsNames _ = return () -- TODO

storeGRHSNames :: GRHS GhcRn (LHsExpr GhcRn) -> StoreM ()
storeGRHSNames (GRHS _ _guardLStmt body) = storeExprNames (unLoc body)
storeGRHSNames _ = return () -- TODO

storeLocalBindingNames :: HsLocalBinds GhcRn -> StoreM ()
storeLocalBindingNames (HsValBinds _ binds) = storeValBindNames binds
storeLocalBindingNames _ = return ()

storeExprNames :: HsExpr GhcRn -> StoreM ()
storeExprNames (HsVar _ id) = storeIdNames False id
-- storeExprNames (HsConLikeOut _ _) = error "Not supported: HsConLikeOut"
storeExprNames (HsLam _ mg) = storeMatchesNames mg
storeExprNames (HsLamCase _ mg) = storeMatchesNames mg
storeExprNames (HsApp _ e1 e2) = storeExprNames (unLoc e1) >> storeExprNames (unLoc e2)
storeExprNames (HsAppType _ e) = storeExprNames (unLoc e)
storeExprNames (OpApp _ e1 e2 e3) = storeExprNames (unLoc e1) >> storeExprNames (unLoc e2) >> storeExprNames (unLoc e3)
storeExprNames (NegApp _ e _) = storeExprNames (unLoc e)
storeExprNames (HsPar _ e) = storeExprNames (unLoc e)
storeExprNames (SectionL _ e1 e2) = storeExprNames (unLoc e1) >> storeExprNames (unLoc e2)
storeExprNames (SectionR _ e1 e2) = storeExprNames (unLoc e1) >> storeExprNames (unLoc e2)
storeExprNames (ExplicitTuple _ es _) = mapM_ (storeTupleArgNames . unLoc) es
storeExprNames (ExplicitSum _ _ _ e) = storeExprNames (unLoc e)
storeExprNames (HsCase _ e mg) = storeMatchesNames mg
storeExprNames (HsIf _ _ e1 e2 e3) = storeExprNames (unLoc e1) >> storeExprNames (unLoc e2) >> storeExprNames (unLoc e3)
storeExprNames (HsMultiIf _ grhs) = mapM_ (storeGRHSNames . unLoc) grhs
storeExprNames (HsLet _ locBinds e) = storeLocalBindingNames (unLoc locBinds) >> storeExprNames (unLoc e)
storeExprNames (HsDo _ _ stmts) = mapM_ (storeStatementNames . unLoc) (unLoc stmts)
storeExprNames (ExplicitList _ _ es) = mapM_ (storeExprNames . unLoc) es
-- storeExprNames (RecordCon _ _ _) = error "Not supported: RecordCon" -- TODO
-- storeExprNames (RecordUpd _ _ _) = error "Not supported: RecordUpd" -- TODO
storeExprNames (ExprWithTySig t e) = do
    storeExprNames (unLoc e)
    storeTypeNames $ unLoc $ hsib_body $ hswc_body t
-- storeExprNames (ArithSeq _ se si) = error "Not supported: ArithSeq" -- TODO
storeExprNames (HsSCC _ _ _ e) = storeExprNames (unLoc e)
storeExprNames (HsCoreAnn _ _ _ e) = storeExprNames (unLoc e)
-- storeExprNames (HsBracket {}) = error "Not supported: HsBracket" -- TODO
-- storeExprNames (HsRnBracketOut {}) = error "Not supported: HsRnBracketOut" -- TODO
-- storeExprNames (HsTcBracketOut {}) = error "Not supported: HsTcBracketOut" -- TODO
storeExprNames (HsSpliceE _ sp) = storeHsSpliceNames sp
-- storeExprNames (HsProc {}) = error "Not supported: HsProc" -- TODO
storeExprNames (HsStatic _ e) = storeExprNames (unLoc e)
storeExprNames (HsArrApp _ e1 e2 _ _) = storeExprNames (unLoc e1) >> storeExprNames (unLoc e2)
-- storeExprNames (HsArrForm {}) = error "Not supported: HsArrForm" -- TODO
storeExprNames (HsBinTick _ _ _ e) = storeExprNames (unLoc e)
storeExprNames (HsTickPragma _ _ _ _ e) = storeExprNames (unLoc e)
storeExprNames (EAsPat _ at e) = storeIdNames True at >> storeExprNames (unLoc e)
storeExprNames (EViewPat _ e1 e2) = storeExprNames (unLoc e1) >> storeExprNames (unLoc e2)
storeExprNames (ELazyPat _ e) = storeExprNames (unLoc e)
storeExprNames (HsWrap _ _ e) = storeExprNames e
storeExprNames _ = return ()

storeHsSpliceNames :: HsSplice GhcRn -> StoreM ()
storeHsSpliceNames (HsTypedSplice _ _ _ e) = storeExprNames (unLoc e)
storeHsSpliceNames (HsUntypedSplice _ _ _ e) = storeExprNames (unLoc e)
storeHsSpliceNames (HsSpliced _ _ spliced) = storeHsSplicedNames spliced
storeHsSpliceNames _ = return ()

storeHsSplicedNames :: HsSplicedThing GhcRn -> StoreM ()
storeHsSplicedNames (HsSplicedExpr e) = storeExprNames e
storeHsSplicedNames (HsSplicedTy t) = storeTypeNames t
storeHsSplicedNames (HsSplicedPat p) = storePatternNames p

storeStatementNames :: StmtLR GhcRn GhcRn (LHsExpr GhcRn) -> StoreM ()
storeStatementNames (LastStmt _ e _ _) = storeExprNames (unLoc e)
storeStatementNames (BindStmt _ p e _ _) = storeExprNames (unLoc e)
storeStatementNames (BodyStmt _ e _ _) = storeExprNames (unLoc e)
storeStatementNames (LetStmt _ locBinds) = storeLocalBindingNames (unLoc locBinds)
-- storeStatementNames (ParStmt {}) = error "Not supported: ParStmt" -- TODO
-- storeStatementNames (TransStmt {}) = error "Not supported: TransStmt" -- TODO
-- storeStatementNames (RecStmt {}) = error "Not supported: RecStmt" -- TODO
storeStatementNames _ = return ()

storePatternNames :: Pat GhcRn -> StoreM ()
storePatternNames (VarPat _ id) = storeIdNames True id
storePatternNames (LazyPat _ pat) = storePatternNames (unLoc pat)
storePatternNames (AsPat _ as pat) = storeIdNames True as >> storePatternNames (unLoc pat)
storePatternNames (ParPat _ pat) = storePatternNames (unLoc pat)
storePatternNames (BangPat _ pat) = storePatternNames (unLoc pat)
storePatternNames (ListPat _ pats) = mapM_ (storePatternNames . unLoc) pats
storePatternNames (TuplePat _ pats _) = mapM_ (storePatternNames . unLoc) pats
storePatternNames (SumPat _ pat _ _) = storePatternNames (unLoc pat)
-- storePatternNames (ConPatIn {}) = error "Not supported: ConPatIn"
-- storePatternNames (ConPatOut {}) = error "Not supported: ConPatOut"
storePatternNames (ViewPat _ e pat) = storeExprNames (unLoc e) >> storePatternNames (unLoc pat)
-- storePatternNames (SplicePat {}) = error "Not supported: SplicePat"
storePatternNames (SigPat ty pat) = do 
    storeTypeNames $ unLoc $ hsib_body $ hswc_body ty
    storePatternNames (unLoc pat)
storePatternNames (CoPat _ _ p _) = storePatternNames p
storePatternNames _ = return ()

storeTupleArgNames :: HsTupArg GhcRn -> StoreM ()
storeTupleArgNames (Present _ e) = storeExprNames (unLoc e)
storeTupleArgNames _ = return ()

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
storeIdNames defined id = do
    liftIO $ putStrLn $ "WARNING " ++ (showSDocUnsafe $ ppr id) ++ " does not have a real src span"


