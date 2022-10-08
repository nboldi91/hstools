{-# LANGUAGE LambdaCase #-}

module Language.Haskell.HsTools.SourcePosition where

import Data.List.Split

data SP a = SP { spLine :: Int, spCol :: Int }
  deriving (Eq, Ord, Show, Read)

data Range a = Range { srStart :: SP a, srEnd :: SP a }
  deriving (Eq, Show, Read)

data SrcDiff orig mod = SrcDiff { sdFrom :: Range orig, sdTo :: Range mod }
  deriving (Eq, Show, Read)

lineDiff :: SP mod -> SP orig ->Int
lineDiff modPos oldPos  = spLine modPos - spLine oldPos

colDiff :: SP mod -> SP orig -> Int
colDiff modPos oldPos = spCol modPos - spCol oldPos

sameLine :: SP a -> SP a -> Bool
sameLine (SP l1 _) (SP l2 _) = l1 == l2

rangeContain :: SP a -> Range a -> Bool
rangeContain pos (Range start end) = start <= pos && pos <= end

-- Zero if inline range
rangeHeight :: Range a -> Int
rangeHeight (Range start end) = spLine end - spLine start

concatRanges :: [Range a] -> Range a
concatRanges [] = error "concatRanges: empty"
concatRanges rngs = Range (minimum $ map srStart rngs) (maximum $ map srEnd rngs)

spAdvanceStr :: SP a -> String -> SP a
spAdvanceStr = foldl spAdvanceChar

spAdvanceChar :: SP a -> Char -> SP a
spAdvanceChar (SP lineNo colNo) = \case
  '\n' -> SP (lineNo + 1) 1
  '\t' -> SP (lineNo + 2) 1 -- tabs will make a mess when further changes arrive
  _ -> SP lineNo (colNo + 1)

rangeChange :: (SP a -> SP a) -> Range a -> Range a
rangeChange f (Range s e) = Range (f s) (f e)

serializeSP :: SP a -> String
serializeSP (SP l c) = show l ++ ":" ++ show c 

serializeRange :: Range a -> String
serializeRange (Range from to) = serializeSP from ++ "-" ++ serializeSP to

serializeDiff :: SrcDiff orig mod -> String
serializeDiff (SrcDiff from to) = serializeRange from ++ " -> " ++ serializeRange to

deserializeSP :: String -> SP a
deserializeSP str =
  case break (== ':') str of
    (l, ':' : c) -> SP (read l) (read c)
    _ -> error $ "deserializeSP: '" ++ str ++ "'"

deserializeRange :: String -> Range a
deserializeRange str =
  case break (== '-') str of
    (from, '-' : to) -> Range (deserializeSP from) (deserializeSP to)
    _ -> error $ "deserializeRange: '" ++ str ++ "'"

deserializeDiff :: String -> SrcDiff orig mod 
deserializeDiff str = case filter (not . null) $ splitOn " -> " str of
  [from, to] -> SrcDiff (deserializeRange from) (deserializeRange to)
  _ -> error $ "deserializeDiff: '" ++ str ++ "'"

-- TODO: remove

castSrcDiff :: SrcDiff orig mod -> SrcDiff orig2 mod2
castSrcDiff (SrcDiff rng1 rng2) = SrcDiff (castRange rng1) (castRange rng2)

castRange :: Range orig -> Range orig2
castRange (Range from to) = Range (castSP from) (castSP to)

castSP :: SP orig -> SP orig2
castSP (SP l c) = SP l c
