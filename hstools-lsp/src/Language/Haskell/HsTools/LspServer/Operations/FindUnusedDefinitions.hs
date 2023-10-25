{-# LANGUAGE TupleSections #-}

module Language.Haskell.HsTools.LspServer.Operations.FindUnusedDefinitions where

import Database.PostgreSQL.Simple

import Data.Function (on)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Language.Haskell.HsTools.Database

findUnusedDefinitions :: Connection -> IO [String]
findUnusedDefinitions conn = do
  mains <- getAllMains conn
  definitionsListList <- getAllDefinitionNodes conn
  let definitions = concat definitionsListList
  references <- getNamesUsedInDefinitions conn
  let refs = L.groupBy ((==) `on` fst) $ L.sortOn fst $ map (\(n,a,b,c,d,e) -> ((a,b,c,d,e), n)) references
  connections <- concat . concat <$> mapM (\pairs -> map (map (, map snd pairs)) <$> getNameOfDefinition conn (fst $ head pairs)) refs
  -- this is a hack because I cannot get the main functions right now (mains is empty)
  return $ (findUnused (concat mains ++ filter (".main@3" `L.isSuffixOf`) definitions) definitions connections)

findUnused :: [String] -> [String] -> [(String, [String])] -> [String]
findUnused startPoints allDefinitions usages =
  S.toList $
    S.fromList allDefinitions S.\\
      graphExtend S.empty startPoints (M.fromListWith (++) usages)

graphExtend :: S.Set String -> [String] -> M.Map String [String] -> S.Set String
graphExtend done [] _ = done
graphExtend done (unprocessed : restUnproc) connections
  | unprocessed `S.member` done = graphExtend done restUnproc connections
  | otherwise = graphExtend (S.insert unprocessed done) (fromMaybe [] (M.lookup unprocessed connections) ++ restUnproc) connections
