{-# OPTIONS_GHC -F -pgmF htfpp #-}

module AddRewriteTests ( htf_thisModulesTests ) where

import qualified Data.Map as M

import Test.Framework
import Test.HUnit.Base (Assertion)

import Language.Haskell.HsTools.LinesDiff
import Language.Haskell.HsTools.Database


test_addRewrite_oneExtraCharBeforeOneChange :: Assertion
test_addRewrite_oneExtraCharBeforeOneChange
  = assertEqual
      ( ["{- INSERTED -}x  = a", "y = b"]
      , M.fromList [(SP 1 1, SourceDiffData (SP 1 1) (DSP 0 14 1)), (SP 1 2, SourceDiffData (SP 1 2) (DSP 0 1 1))]
      )
      $ addExtraChange
          compiledLines
          (SourceRewrite (SP 1 1) (SP 1 1) "{- INSERTED -}")
          (changedLines, originalDiffs)

test_addRewrite_oneExtraCharAfterOneChange :: Assertion
test_addRewrite_oneExtraCharAfterOneChange
  = assertEqual
      ( ["x  ={- INSERTED -} a", "y = b"]
      , M.fromList [(SP 1 2, SourceDiffData (SP 1 2) (DSP 0 1 1)), (SP 1 4, SourceDiffData (SP 1 4) (DSP 0 14 1))]
      )
      $ addExtraChange
          compiledLines
          (SourceRewrite (SP 1 5) (SP 1 5) "{- INSERTED -}")
          (changedLines, originalDiffs)

test_addRewrite_mergeWithOneChange :: Assertion
test_addRewrite_mergeWithOneChange
  = assertEqual
      ( ["x{- INSERTED -}  = a", "y = b"]
      , M.fromList [(SP 1 2, SourceDiffData (SP 1 2) (DSP 0 15 1))]
      )
      $ addExtraChange
          compiledLines
          (SourceRewrite (SP 1 2) (SP 1 2) "{- INSERTED -}")
          (changedLines, originalDiffs)

compiledLines :: [String]
compiledLines = ["x = a", "y = b"]

changedLines :: [String]
changedLines = ["x  = a", "y = b"]

test_addRewrite_mergeWithTwoChanges :: Assertion
test_addRewrite_mergeWithTwoChanges
  = assertEqual
      ( ["x  ***  a", "y = b"]
      , M.fromList [(SP 1 3, SourceDiffData (SP 1 4) (DSP 0 5 1))]
      )
      $ addExtraChange
          compiledLines
          (SourceRewrite (SP 1 4) (SP 1 5) "***")
          (twiceChangedLines, twoOriginalDiffs)

-- rewrite 1:2-1:2 --> 1:2-1:3
originalDiffs :: SourceDiffs
originalDiffs = M.fromList [(SP 1 2, SourceDiffData (SP 1 2) (DSP 0 1 1))]

twiceChangedLines :: [String]
twiceChangedLines = ["x  =  a", "y = b"]

-- rewrite 1:3-1:3 --> 1:3-1:4, 1:4-1:4 --> 1:4-1:5
twoOriginalDiffs :: SourceDiffs
twoOriginalDiffs = M.fromList [(SP 1 3, SourceDiffData (SP 1 3) (DSP 0 1 1)), (SP 1 4, SourceDiffData (SP 1 4) (DSP 0 1 1))]

-- TODO: line change case

-- TODO: revert change