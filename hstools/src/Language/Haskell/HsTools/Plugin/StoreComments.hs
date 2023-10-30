{-# LANGUAGE TupleSections #-}

module Language.Haskell.HsTools.Plugin.StoreComments where

import ApiAnnotation
import SrcLoc

import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Language.Haskell.HsTools.Plugin.Types
import Language.Haskell.HsTools.SourcePosition

findDefinitionOfComments :: Int -> Map.Map SrcSpan [Located AnnotationComment] -> [(ParseRecord, Int)] -> [(Int, Int, String)]
findDefinitionOfComments moduleId commMap records =
  map (\(defId, text) -> (moduleId, defId, text))
    $ definitionsComments
      (catMaybes $ map (\(L l c) -> (, getAnnString c) <$> srcSpanToNodePos l) $ concat $ Map.elems commMap)
      (Map.fromList $ map (\(ParseDefinitionRecord _ (Range (SP sr sc) _), defId) -> ((sr, sc), defId)) records)
      (Map.fromList $ map (\(ParseDefinitionRecord _ (Range _ (SP er ec)), defId) -> ((er, ec), defId)) records)

definitionsComments :: [(Range NodePos, String)] -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int -> [(Int, String)]
definitionsComments comments defStartMap defEndMap = catMaybes $ map (findCommentedDef defStartMap defEndMap) comments

findCommentedDef :: Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int -> (Range NodePos, String) -> Maybe (Int, String)
findCommentedDef defStartMap defEndMap (np, comm)
  | "-- |" `isPrefixOf` comm || "{- |" `isPrefixOf` comm || "{-|" `isPrefixOf` comm =
    fmap ((,comm) . fst) $ Map.minView $ snd $ Map.split (endLine np, endCol np) defStartMap
  | "-- ^" `isPrefixOf` comm || "{- ^" `isPrefixOf` comm || "{-^" `isPrefixOf` comm =
    fmap ((,comm) . fst) $ Map.maxView $ fst $ Map.split (startLine np, startCol np) defEndMap
  | otherwise = Nothing

getAnnString :: AnnotationComment -> String
getAnnString (AnnDocCommentNext str) = str
getAnnString (AnnDocCommentPrev str) = str
getAnnString (AnnDocCommentNamed str) = str
getAnnString (AnnDocSection _ str) = str
getAnnString (AnnDocOptions str) = str
getAnnString (AnnLineComment str) = str
getAnnString (AnnBlockComment str) = str
