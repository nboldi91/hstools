module Language.Haskell.HsTools.FileLines (FileLines(..), Rewrite(..), concatFileLines, toFileLines, fromFileLines, applySourceDiff, takeRange) where

import Data.List
import Data.List.Split

import Language.Haskell.HsTools.SourcePosition

newtype FileLines = FileLines { flData :: [String] }
  deriving (Show, Eq)

data Rewrite orig = Rewrite { rwRange :: Range orig, rwReplacement :: String }
  deriving (Eq, Show, Read)

concatFileLines :: [FileLines] -> FileLines
concatFileLines = FileLines . foldl (\a b -> init a ++ [last a ++ head b] ++ tail b) [""] . filter (not . null) . map flData

toFileLines :: String -> FileLines
toFileLines = FileLines . splitOn "\n"

fromFileLines :: FileLines -> String
fromFileLines = intercalate "\n" . flData

applySourceDiff :: Rewrite orig -> FileLines -> FileLines
applySourceDiff (Rewrite (Range start end) replacement) (FileLines lns) = concatFileLines $ [FileLines before, toFileLines replacement, FileLines after]
  where -- could be more optimized
    before = takeLines (spLine start - 1) (spCol start - 1) lns
    after = dropLines (spLine end - 1) (spCol end - 1) lns

takeRange :: Range a -> FileLines -> FileLines
takeRange (Range start end) (FileLines lns) = FileLines $ toEnd $ dropLines (spLine start - 1) (spCol start - 1) lns
  where toEnd = if sameLine start end
                  then takeLines 0 (spCol end - spCol start)
                  else takeLines (spLine end - spLine start) (spCol end - 1)

dropLines :: Int -> Int -> [String] -> [String]
dropLines n c (_ : lines) | n > 0 = dropLines (n - 1) c lines
dropLines _ c (line : lines) = drop c line : lines
dropLines _ _ lines = lines

takeLines :: Int -> Int -> [String] -> [String]
takeLines n c (line : lines) | n > 0 = line : takeLines (n - 1) c lines
takeLines _ c (line : _) = [take c line]
takeLines _ _ _ = []
