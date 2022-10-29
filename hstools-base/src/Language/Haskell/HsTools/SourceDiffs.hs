{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Language.Haskell.HsTools.SourceDiffs
  ( SourceDiffs, emptyDiffs, isEmptyDiffs, addExtraChange, createSourceDiffs
  , originalToNewRange, originalToNewRangeStrict, originalToNewRangeOptimist, originalToNewPos
  , newToOriginalRange, newToOriginalRangeStrict, newToOriginalPos
  , serializeSourceDiffs, deserializeSourceDiffs
  )
  where

import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Algorithm.Diff

import Language.Haskell.HsTools.SourcePosition
import Language.Haskell.HsTools.FileLines

data SourceDiffs orig mod = SourceDiffs
  { origKeyedMap :: Map.Map (SP orig) (SrcDiff orig mod)
  , modKeyedMap :: Map.Map (SP mod) (SrcDiff orig mod)
  } deriving (Show, Eq)

newToOriginalRange :: SourceDiffs orig mod -> Range mod -> Range orig
newToOriginalRange diffs (Range start end) = Range newStart newEnd
  where
    newStart = either srStart id $ newToOriginalPos diffs start
    newEnd = either srEnd id $ newToOriginalPos diffs end

newToOriginalRangeStrict :: SourceDiffs orig mod -> Range mod -> Maybe (Range orig)
newToOriginalRangeStrict diffs (Range start end)
  = case (newToOriginalPos diffs start, newToOriginalPos diffs end) of
      (Right newStart, Right newEnd) -> Just $ Range newStart newEnd
      _ -> Nothing

newToOriginalPos :: SourceDiffs orig mod -> SP mod -> Either (Range orig) (SP orig)
newToOriginalPos diffs pos = maybe noChange apply (changeBeforeNew pos diffs) $ pos
  where
    noChange SP{..} = Right SP{..}

    apply :: SrcDiff orig mod -> SP mod -> Either (Range orig) (SP orig)
    apply (SrcDiff oldRange@(Range _ oldEnd) (Range _ newEnd)) pos@(SP l c)
      | pos <= newEnd = Left oldRange
      | pos `sameLine` newEnd = Right $ SP (spLine oldEnd) (c - colDiff newEnd oldEnd)
      | otherwise = Right $ SP (l - lineDiff newEnd oldEnd) c

originalToNewRange :: SourceDiffs orig mod -> Range orig -> Range mod
originalToNewRange diffs (Range start end) = Range newStart newEnd
  where
    newStart = either srStart id $ originalToNewPos diffs start
    newEnd = either srEnd id $ originalToNewPos diffs end

originalToNewRangeOptimist :: SourceDiffs orig mod -> Range orig -> Maybe (Range mod)
originalToNewRangeOptimist diffs (Range start end)
  | newStart < newEnd = Just $ Range newStart newEnd
  | otherwise = Nothing
  where
    newStart = either srEnd id $ originalToNewPos diffs start
    newEnd = either srStart id $ originalToNewPos diffs end

originalToNewRangeStrict :: SourceDiffs orig mod -> Range orig -> Maybe (Range mod)
originalToNewRangeStrict diffs (Range start end)
  = case (originalToNewPos diffs start, originalToNewPos diffs end) of
      (Right newStart, Right newEnd) -> Just $ Range newStart newEnd
      _ -> Nothing

originalToNewPos :: SourceDiffs orig mod -> SP orig -> Either (Range mod) (SP mod)
originalToNewPos diffs pos = maybe noChange apply (changeBeforeOriginal pos diffs) $ pos
  where
    noChange SP{..} = Right SP{..}

    apply :: SrcDiff orig mod -> SP orig -> Either (Range mod) (SP mod)
    apply (SrcDiff (Range _ oldEnd) newRng@(Range _ newEnd)) pos@(SP l c)
      | pos <= oldEnd = Left newRng
      | pos `sameLine` oldEnd = Right $ SP (spLine newEnd) (c + colDiff newEnd oldEnd)
      | otherwise = Right $ SP (l + lineDiff newEnd oldEnd) c

changeBeforeOriginal :: SP orig -> SourceDiffs orig mod -> Maybe (SrcDiff orig mod)
changeBeforeOriginal pos diffs = maybe (fmap fst $ Map.maxView before) Just exact
  where (before, exact, _) = Map.splitLookup pos $ origKeyedMap diffs

changeBeforeNew :: SP mod -> SourceDiffs orig mod -> Maybe (SrcDiff orig mod)
changeBeforeNew pos diffs = maybe (fmap fst $ Map.maxView before) Just exact
  where (before, exact, _) = Map.splitLookup pos $ modKeyedMap diffs

isEmptyDiffs :: SourceDiffs orig mod -> Bool
isEmptyDiffs (SourceDiffs m _) = Map.null m

-- output is sorted
fromSourceDiffs :: SourceDiffs orig mod -> [SrcDiff orig mod]
fromSourceDiffs (SourceDiffs m _) = map snd $ Map.toAscList m

-- * Creating source diffs

emptyDiffs :: SourceDiffs orig mod
emptyDiffs = SourceDiffs Map.empty Map.empty

singletonDiffs :: Range orig -> Range mod -> SourceDiffs orig mod
singletonDiffs orig mod
  = SourceDiffs (Map.singleton (srStart orig) diff) (Map.singleton (srStart mod) diff)
  where diff = SrcDiff orig mod

-- | Precondition: input must be sorted
mkSourceDiffs :: [SrcDiff orig mod] -> SourceDiffs orig mod
mkSourceDiffs diffs
  = SourceDiffs
      (Map.fromAscList $ map (\d -> (srStart $ sdFrom d, d)) diffs)
      (Map.fromAscList $ map (\d -> (srStart $ sdTo d, d)) diffs)

-- to get the best possible results we re-diff the part of the source code that is affected by the change
-- extending to encompass all existing changes around it
addExtraChange :: FileLines -> Rewrite mod -> (FileLines, SourceDiffs orig mod) -> (FileLines, SourceDiffs orig mod)
addExtraChange compiledContent rewrite@(Rewrite modRange replacement) (actualContent, diffs) = (newContent, newDiffs)
  where
    newDiffs
      = if not isComplexDiff && isEmptyDiffs affectedDiffs
          then adjustDiff modRange newRange diffs `unionDiffs` singletonDiffs origRange newRange
          else beforeDiffs `unionDiffs` adjustedAfterDiffs `unionDiffs` reDiffs   
    isComplexDiff = length replacement > 1 || (length replacement >= 1 && not (isEmptyRange modRange))
    adjustedAfterDiffs
      = maybe id (\(SrcDiff from to) -> adjustDiff from to) lastReDiff $ adjustByLastAffectedDiff afterDiffs
    adjustByLastAffectedDiff = maybe id (\d -> adjustDiff (sdTo d) (sdFrom d)) lastAffectedDiffMaybe
    lastReDiff = fmap (castSrcDiff . fst) $ Map.maxView $ modKeyedMap reDiffs
    lastAffectedDiffMaybe
      = fmap (castSrcDiff . fst) $ Map.maxView $ modKeyedMap affectedDiffs
    reDiffs = createSourceDiffs
                (srStart origRangeToReDiff)
                (srStart newestRangeToReDiff)
                (fromFileLines $ takeRange origRangeToReDiff compiledContent)
                (fromFileLines $ takeRange newestRangeToReDiff newContent)
    newContent = applySourceDiff rewrite actualContent
    newestRangeToReDiff = originalToNewRange rewriteDiff $ originalToNewRange diffs origRangeToReDiff
    origRangeToReDiff = concatRanges $ origRange : map sdFrom (Map.elems $ modKeyedMap affectedDiffs)
    (beforeDiffs, affectedDiffs, afterDiffs) = touchingChanges diffs modRange
    rewriteDiff = singletonDiffs modRange newRange
    newRange = Range (srStart modRange) (spAdvanceStr (srStart modRange) replacement) 
    origRange = newToOriginalRange diffs modRange

touchingChanges :: SourceDiffs orig mod -> Range mod -> (SourceDiffs orig mod, SourceDiffs orig mod, SourceDiffs orig mod)
touchingChanges diffs (Range start end)
  = (before, touching, after)
  where 
    (before, touching) = case firstNotTouching of
      Just fnt -> breakDiff (srStart $ sdTo fnt) notAfter
      Nothing -> (emptyDiffs, notAfter)
    firstNotTouching = listToMaybe $ dropWhile ((start <=) . srEnd . sdTo) $ map snd $ Map.toDescList $ modKeyedMap notAfter
    (notAfter, after) = breakDiff end diffs

adjustDiff :: Range mod -> Range mod -> SourceDiffs orig mod -> SourceDiffs orig mod
adjustDiff mod@(Range _ modEnd) replaced (SourceDiffs {..})
  -- in this case we only need to update the changes in the current line, this should be the usual case if we have small changes
  | rangeHeight mod == rangeHeight replaced
  = SourceDiffs (origKeyedMap `unionTakeSecond` newOrigKeyedVals) (modKeyedMapObsoletesRemoved `unionTakeSecond` newModKeyedVals)
  where
    (SourceDiffs newOrigKeyedVals newModKeyedVals) = mkSourceDiffs updatedValues

    -- source position update function
    f = updateSP replaced mod

    modKeyedMapObsoletesRemoved = foldr Map.delete modKeyedMap $ map (srStart . sdTo) affectedValues
    
    updatedValues = map (\(SrcDiff r1 r2) -> SrcDiff r1 (rangeChange f r2)) affectedValues
    affectedValues = map snd $ takeWhile ((spLine modEnd ==) . spLine . srStart . sdTo . snd) $ Map.toAscList after

    (_, after) = Map.split modEnd modKeyedMap

    -- flipped the unions for efficiency, but base union prefers the left side when keys collide
    unionTakeSecond :: Ord a => Map.Map a b -> Map.Map a b -> Map.Map a b 
    unionTakeSecond = Map.unionWithKey (\_ _ r -> r)

adjustDiff mod@(Range _ modEnd) replaced diffs
  = notAfter `unionDiffs` SourceDiffs newOrigKeyedVals newModAfter
  where
    newOrigKeyedVals = Map.map (\(SrcDiff r1 r2) -> (SrcDiff r1 (rangeChange f r2))) $ origKeyedMap after
    newModAfter = Map.mapKeysMonotonic f $ Map.map (\(SrcDiff r1 r2) -> (SrcDiff r1 (rangeChange f r2))) $ modKeyedMap after

    -- source position update function
    f = updateSP replaced mod

    (notAfter, after) = breakDiff modEnd diffs

breakDiff :: SP mod -> SourceDiffs orig mod -> (SourceDiffs orig mod, SourceDiffs orig mod)
breakDiff sp (SourceDiffs {..}) = (SourceDiffs origNotAfter modNotAfter, SourceDiffs origAfter modAfter)
  where
    (origNotAfter, origAfter) = case Map.toDescList modNotAfter of
      (_, first):_ -> splitWithExact (srStart $ sdFrom first) origKeyedMap
      [] -> (Map.empty, origKeyedMap)
    (modNotAfter, modAfter) = splitWithExact sp modKeyedMap

-- The first map contains keys <= k, the second keys > k
splitWithExact :: Ord k => k -> Map.Map k v -> (Map.Map k v, Map.Map k v)
splitWithExact k m = (before `Map.union` maybe Map.empty (Map.singleton k) exact, after)
  where (before, exact, after) = Map.splitLookup k m

updateSP :: Range rep -> Range orig -> SP orig -> SP orig
updateSP replaced@(Range _ repEnd) original@(Range _ origEnd) sp@(SP l c)
  | sameLine sp origEnd = SP l (c + spCol repEnd - spCol origEnd)
  | otherwise = SP (l + rangeHeight replaced - rangeHeight original) c

unionDiffs :: SourceDiffs orig mod -> SourceDiffs orig mod -> SourceDiffs orig mod
unionDiffs (SourceDiffs orig1 mod1) (SourceDiffs orig2 mod2) = SourceDiffs (orig1 `Map.union` orig2) (mod1 `Map.union` mod2)

createSourceDiffs :: SP orig -> SP mod -> String -> String -> SourceDiffs orig mod
createSourceDiffs from to original modified = mkSourceDiffs $ go from to $ getGroupedDiff original modified
  where
    go :: SP orig -> SP mod -> [Diff String] -> [SrcDiff orig mod]
    go from to (Both x _ : rest)
      | (from', to') <- spAdvanceStrs (from, to) x
      = go from' to' rest
    go from to (First a : Second b : rest) = calculateReplacement from to a b rest
    go from to (Second b : First a : rest) = calculateReplacement from to a b rest
    go from to (First a : rest) = calculateReplacement from to a "" rest
    go from to (Second b : rest) = calculateReplacement from to "" b rest
    go _ _ [] = []

    calculateReplacement from to replaced replacement rest = 
      SrcDiff (Range from from') (Range to to') : go from' to' rest
      where
        from' = spAdvanceStr from replaced
        to' = spAdvanceStr to replacement

    spAdvanceStrs :: (SP a, SP b) -> String -> (SP a, SP b)
    spAdvanceStrs = foldl (\(p1, p2) c -> (spAdvanceChar p1 c, spAdvanceChar p2 c))

-- * Serialization


serializeSourceDiffs :: SourceDiffs orig mod -> String
serializeSourceDiffs = intercalate " , " . map serializeDiff . fromSourceDiffs

deserializeSourceDiffs :: String -> SourceDiffs orig mod
deserializeSourceDiffs = mkSourceDiffs . map deserializeDiff . filter (not . null) . splitOn " , "
