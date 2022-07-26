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
    ((), names) <- runWriterT (runReaderT (store $ hs_valds gr) (StoreContext moduleName))
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

data UseDefine = Use | Define

class NamePhase p where
    storeName :: UseDefine -> Located (IdP (GhcPass p)) -> StoreM ()

type IsGhcPass p = (NamePhase p, HaskellAst (XExprWithTySig (GhcPass p)), HaskellAst (XSigPat (GhcPass p)))

instance NamePhase Renamed where
    storeName ud (L (RealSrcSpan span) id) = do
        let defined = case ud of Define -> True; Use -> False
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
    storeName _ id = do
        liftIO $ putStrLn $ "WARNING " ++ (showSDocUnsafe $ ppr id) ++ " does not have a real src span"


class HaskellAst a where
    store :: a -> StoreM ()

instance HaskellAst a => HaskellAst (Located a) where
    store = store . unLoc

instance HaskellAst a => HaskellAst [a] where
    store = mapM_ store

instance HaskellAst a => HaskellAst (Bag a) where
    store = mapBagM_ store

instance HaskellAst a => HaskellAst (HsWildCardBndrs (GhcPass p) a) where
    store = store . hswc_body

instance HaskellAst a => HaskellAst (HsImplicitBndrs (GhcPass p) a) where
    store = store . hsib_body

instance IsGhcPass p => HaskellAst (HsValBinds (GhcPass p)) where
    store (ValBinds _ binds sigs) = store sigs >> store binds
    store (XValBindsLR (NValBinds binds sigs)) = store sigs >> store (map snd binds)

instance IsGhcPass p => HaskellAst (Sig (GhcPass p)) where
    store (TypeSig _ ids types) = mapM_ (storeName @p Define) ids >> store types
    store _ = return () -- TODO

instance IsGhcPass p => HaskellAst (HsType (GhcPass p)) where
    store (HsForAllTy _ vars body) = store vars >> store body
    store (HsQualTy _ ctx body) = store body
    store (HsTyVar _ _ id) = storeName @p Use id
    store (HsAppTy _ lhs rhs) = store lhs >> store rhs
    store (HsFunTy _ lhs rhs) = store lhs >> store rhs
    store (HsListTy _ t) = store t
    store (HsTupleTy _ _ ts) = store ts
    store (HsSumTy _ ts) = store ts
    store (HsOpTy _ lhs op rhs) = store lhs >> storeName @p Use op >> store rhs
    store (HsParTy _ t) = store t
    store (HsKindSig _ t k) = store t >> store k
    store (HsBangTy _ _ t) = store t
    -- store (HsRecTy {}) = error "Not supported: HsRecTy"
    store (HsExplicitListTy _ _ ts) = store ts
    store (HsExplicitTupleTy _ ts) = store ts
    store _ = return ()

instance IsGhcPass p => HaskellAst (HsTyVarBndr (GhcPass p)) where
    store (UserTyVar _ id) = storeName @p Define id
    store (KindedTyVar _ id kind) = do
        storeName @p Define id
        store kind -- kinds are just types
    store (XTyVarBndr _) = return ()

instance IsGhcPass p => HaskellAst (HsBindLR (GhcPass p) (GhcPass p)) where
    store (FunBind _ id matches _ _) = do
        storeName @p Define id
        store matches
    store _ = return () -- TODO

instance (IsGhcPass p, HaskellAst a) => HaskellAst (MatchGroup (GhcPass p) a) where 
    store (MG _ alts _) = store alts
    store _ = return () -- TODO

instance (IsGhcPass p, HaskellAst a) => HaskellAst (Match (GhcPass p) a) where 
    store (Match _ _ _pats grhss) = store grhss
    store _ = return () -- TODO

instance (IsGhcPass p, HaskellAst a) => HaskellAst (GRHSs (GhcPass p) a) where 
    store (GRHSs _ grhs _localBinds) = store grhs
    store _ = return () -- TODO

instance (IsGhcPass p, HaskellAst a) => HaskellAst (GRHS (GhcPass p) a) where 
    store (GRHS _ _guards body) = store body
    store _ = return () -- TODO

instance IsGhcPass p => HaskellAst (HsLocalBinds (GhcPass p)) where 
    store (HsValBinds _ binds) = store binds
    store _ = return () -- TODO

instance IsGhcPass p => HaskellAst (HsExpr (GhcPass p)) where 
    store (HsVar _ id) = storeName @p Use id
    -- store (HsConLikeOut _ _) = error "Not supported: HsConLikeOut"
    store (HsLam _ mg) = store mg
    store (HsLamCase _ mg) = store mg
    store (HsApp _ e1 e2) = store e1 >> store e2
    store (HsAppType _ e) = store e
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
    -- store (RecordCon _ _ _) = error "Not supported: RecordCon" -- TODO
    -- store (RecordUpd _ _ _) = error "Not supported: RecordUpd" -- TODO
    store (ExprWithTySig t e) = store e >> store t
    -- store (ArithSeq _ se si) = error "Not supported: ArithSeq" -- TODO
    store (HsSCC _ _ _ e) = store e
    store (HsCoreAnn _ _ _ e) = store e
    -- store (HsBracket {}) = error "Not supported: HsBracket" -- TODO
    -- store (HsRnBracketOut {}) = error "Not supported: HsRnBracketOut" -- TODO
    -- store (HsTcBracketOut {}) = error "Not supported: HsTcBracketOut" -- TODO
    store (HsSpliceE _ sp) = store sp
    -- store (HsProc {}) = error "Not supported: HsProc" -- TODO
    store (HsStatic _ e) = store e
    store (HsArrApp _ e1 e2 _ _) = store e1 >> store e2
    -- store (HsArrForm {}) = error "Not supported: HsArrForm" -- TODO
    store (HsBinTick _ _ _ e) = store e
    store (HsTickPragma _ _ _ _ e) = store e
    store (EAsPat _ at e) = storeName @p Define at >> store e
    store (EViewPat _ e1 e2) = store e1 >> store e2
    store (ELazyPat _ e) = store e
    store (HsWrap _ _ e) = store e
    store _ = return ()

instance IsGhcPass p => HaskellAst (HsSplice (GhcPass p)) where 
    store (HsTypedSplice _ _ _ e) = store e
    store (HsUntypedSplice _ _ _ e) = store e
    store (HsSpliced _ _ spliced) = store spliced
    store _ = return ()

instance IsGhcPass p => HaskellAst (HsSplicedThing (GhcPass p)) where 
    store (HsSplicedExpr e) = store e
    store (HsSplicedTy t) = store t
    store (HsSplicedPat p) = store p

instance (IsGhcPass p, HaskellAst a) => HaskellAst (StmtLR (GhcPass p) (GhcPass p) a) where 
    store (LastStmt _ e _ _) = store e
    store (BindStmt _ p e _ _) = store p >> store e
    store (BodyStmt _ e _ _) = store e
    store (LetStmt _ locBinds) = store locBinds
    -- store (ParStmt {}) = error "Not supported: ParStmt" -- TODO
    -- store (TransStmt {}) = error "Not supported: TransStmt" -- TODO
    -- store (RecStmt {}) = error "Not supported: RecStmt" -- TODO
    store _ = return ()

instance IsGhcPass p => HaskellAst (Pat (GhcPass p)) where 
    store (VarPat _ id) = storeName @p Define id
    store (LazyPat _ pat) = store pat
    store (AsPat _ as pat) = storeName @p Define as >> store pat
    store (ParPat _ pat) = store pat
    store (BangPat _ pat) = store pat
    store (ListPat _ pats) = store pats
    store (TuplePat _ pats _) = store pats
    store (SumPat _ pat _ _) = store pat
    -- store (ConPatIn {}) = error "Not supported: ConPatIn"
    -- store (ConPatOut {}) = error "Not supported: ConPatOut"
    store (ViewPat _ e pat) = store e >> store pat
    -- store (SplicePat {}) = error "Not supported: SplicePat"
    store (SigPat ty pat) = store ty >> store pat
    store (CoPat _ _ p _) = store p
    store _ = return ()

instance IsGhcPass p => HaskellAst (HsTupArg (GhcPass p)) where 
    store (Present _ e) = store e
    store _ = return ()

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


