{-# OPTIONS_GHC -F -pgmF htfpp #-}
module AddRewriteTests ( htf_thisModulesTests ) where

import qualified Data.Map as M

import Test.Framework
import Test.HUnit.Base (Assertion)

import Language.Haskell.HsTools.SourceDiffs
import Language.Haskell.HsTools.FileLines
import Language.Haskell.HsTools.SourcePosition
import Language.Haskell.HsTools.Database

data Orig
data Mod

test_addRewrite_oneExtraCharBeforeOneChange :: Assertion
test_addRewrite_oneExtraCharBeforeOneChange
  = assertEqual
      ( FileLines ["{- INSERTED -}x  = a", "y = b"], deserializeSourceDiffs "1:1-1:1 -> 1:1-1:15 , 1:2-1:2 -> 1:16-1:17" )
      $ addExtraChange
          (FileLines ["x = a", "y = b"])
          (Rewrite (deserializeRange "1:1-1:1") "{- INSERTED -}")
          (FileLines ["x  = a", "y = b"], deserializeSourceDiffs "1:2-1:2 -> 1:2-1:3")

test_addRewrite_oneExtraCharAfterOneChange :: Assertion
test_addRewrite_oneExtraCharAfterOneChange
  = assertEqual
      ( FileLines ["x  ={- INSERTED -} a", "y = b"], deserializeSourceDiffs "1:2-1:2 -> 1:2-1:3 , 1:4-1:4 -> 1:5-1:19" )
      $ addExtraChange
          (FileLines ["x = a", "y = b"])
          (Rewrite (deserializeRange "1:5-1:5") "{- INSERTED -}")
          (FileLines ["x  = a", "y = b"], deserializeSourceDiffs "1:2-1:2 -> 1:2-1:3")

test_addRewrite_mergeWithOneChange :: Assertion
test_addRewrite_mergeWithOneChange
  = assertEqual
      ( FileLines ["x{- INSERTED -}  = a", "y = b"], deserializeSourceDiffs "1:2-1:2 -> 1:2-1:17" )
      $ addExtraChange
          (FileLines ["x = a", "y = b"])
          (Rewrite (deserializeRange "1:2-1:2") "{- INSERTED -}")
          (FileLines ["x  = a", "y = b"], deserializeSourceDiffs "1:2-1:2 -> 1:2-1:3")

test_addRewrite_mergeWithTwoChanges :: Assertion
test_addRewrite_mergeWithTwoChanges
  = assertEqual
      ( FileLines ["x  ***  a", "y = b"], deserializeSourceDiffs "1:3-1:4 -> 1:3-1:8" )
      $ addExtraChange
          (FileLines ["x = a", "y = b"])
          (Rewrite (deserializeRange "1:4-1:5") "***")
          (FileLines ["x  =  a", "y = b"], deserializeSourceDiffs "1:3-1:3 -> 1:3-1:4 , 1:4-1:4 -> 1:5-1:6")

test_addRewrite_mergeDeletes :: Assertion
test_addRewrite_mergeDeletes
  = assertEqual
      ( FileLines ["x = a"], deserializeSourceDiffs "1:2-1:5 -> 1:2-1:2" )
      $ addExtraChange
          (FileLines ["x    = a"])
          (Rewrite (deserializeRange "1:2-1:4") "")
          (FileLines ["x   = a"], deserializeSourceDiffs "1:3-1:4 -> 1:3-1:3")

test_addRewrite_mergeBeforeChanges :: Assertion
test_addRewrite_mergeBeforeChanges
  = assertEqual
      ( FileLines ["", "", "", "", "x = a"], deserializeSourceDiffs "1:1-1:1 -> 1:1-3:1 , 2:1-2:1 -> 4:1-5:1" )
      $ addExtraChange
          (FileLines ["", "x = a"])
          (Rewrite (deserializeRange "1:1-1:1") "\n")
          (FileLines ["", "", "", "x = a"], deserializeSourceDiffs "1:1-1:1 -> 1:1-2:1 , 2:1-2:1 -> 3:1-4:1")

test_addRewrite_revertAddition :: Assertion
test_addRewrite_revertAddition
  = assertEqual
      ( FileLines ["x = a"]
      , deserializeSourceDiffs ""
      )
      $ addExtraChange
          (FileLines ["x = a"])
          (Rewrite (deserializeRange "1:1-1:2") "")
          (FileLines [" x = a"], deserializeSourceDiffs "1:1-1:1 -> 1:1-1:2")

test_addRewrite_revertDeletion :: Assertion
test_addRewrite_revertDeletion
  = assertEqual ( FileLines [" x = a"], deserializeSourceDiffs "" )
      $ addExtraChange
          (FileLines [" x = a"])
          (Rewrite (deserializeRange "1:1-1:1") " ")
          (FileLines ["x = a"], deserializeSourceDiffs "1:1-1:2 -> 1:1-1:1")

test_addRewrite_simplified :: Assertion
test_addRewrite_simplified
  = assertEqual ( FileLines ["xy = a"], deserializeSourceDiffs "1:2-1:2 -> 1:2-1:3" )
      $ addExtraChange
          (FileLines ["x = a"])
          (Rewrite (deserializeRange "1:1-1:4") "xy =")
          (FileLines ["x = a"], deserializeSourceDiffs "")
