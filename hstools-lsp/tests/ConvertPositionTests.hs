{-# OPTIONS_GHC -F -pgmF htfpp #-}
module ConvertPositionTests ( htf_thisModulesTests ) where

import qualified Data.Map as Map

import Test.Framework
import Test.HUnit.Base (Assertion)

import Language.Haskell.HsTools.LinesDiff
import Language.Haskell.HsTools.Database

-----------------------
---- Add characters 

test_newToOriginal_oneRewrite :: Assertion
test_newToOriginal_oneRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 2)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 3)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 4)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 5)
    assertEqual (Right $ SP 1 5) (f $ SP 1 6)
    assertEqual (Right $ SP 2 3) (f $ SP 2 3)
  where f = newToOriginalPos oneRewrite

test_originalToNew_oneRewrite :: Assertion
test_originalToNew_oneRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 5)) (f $ SP 1 2)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 5)) (f $ SP 1 3)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 5)) (f $ SP 1 4)
    assertEqual (Right $ SP 1 6) (f $ SP 1 5)
    assertEqual (Right $ SP 1 7) (f $ SP 1 6)
    assertEqual (Right $ SP 2 3) (f $ SP 2 3)
  where f = originalToNewPos oneRewrite

-- rewrite 1:2-1:4 --> 1:2-1:5
oneRewrite :: SourceDiffs
oneRewrite = Map.fromList [(SP 1 2, SourceDiffData (SP 1 4) (DSP 0 3 1))]

-----------------
---- Add line 

test_newToOriginal_addLineRewrite :: Assertion
test_newToOriginal_addLineRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 2)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 3)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 4)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 2 1)
    assertEqual (Right $ SP 1 5) (f $ SP 2 2)
    assertEqual (Right $ SP 1 6) (f $ SP 2 3)
    assertEqual (Right $ SP 2 1) (f $ SP 3 1)
    assertEqual (Right $ SP 2 2) (f $ SP 3 2)
  where f = newToOriginalPos addLineRewrite

test_originalToNew_addLineRewrite :: Assertion
test_originalToNew_addLineRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 2 1)) (f $ SP 1 2)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 2 1)) (f $ SP 1 3)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 2 1)) (f $ SP 1 4)
    assertEqual (Right $ SP 2 2) (f $ SP 1 5)
    assertEqual (Right $ SP 3 1) (f $ SP 2 1)
  where f = originalToNewPos addLineRewrite

-- rewrite 1:2-1:4 --> 1:2-2:1
addLineRewrite :: SourceDiffs
addLineRewrite = Map.fromList [(SP 1 2, SourceDiffData (SP 1 4) (DSP 1 (-1) 2))]

--------------------------------
---- Add line and characters

test_newToOriginal_addLineCharRewrite :: Assertion
test_newToOriginal_addLineCharRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 2)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 3)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 4)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 2 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 2 3)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 2 4)
    assertEqual (Right $ SP 1 5) (f $ SP 2 5)
    assertEqual (Right $ SP 1 6) (f $ SP 2 6)
    assertEqual (Right $ SP 2 1) (f $ SP 3 1)
    assertEqual (Right $ SP 2 2) (f $ SP 3 2)
  where f = newToOriginalPos addLineCharRewrite

test_originalToNew_addLineCharRewrite :: Assertion
test_originalToNew_addLineCharRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 2 4)) (f $ SP 1 2)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 2 4)) (f $ SP 1 3)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 2 4)) (f $ SP 1 4)
    assertEqual (Right $ SP 2 5) (f $ SP 1 5)
    assertEqual (Right $ SP 2 6) (f $ SP 1 6)
    assertEqual (Right $ SP 3 1) (f $ SP 2 1)
    assertEqual (Right $ SP 3 2) (f $ SP 2 2)
  where f = originalToNewPos addLineCharRewrite

-- rewrite 1:2-1:4 --> 1:2-2:4
addLineCharRewrite :: SourceDiffs
addLineCharRewrite = Map.fromList [(SP 1 2, SourceDiffData (SP 1 4) (DSP 1 2 2))]

--------------------
---- Delete range

test_newToOriginal_deleteRewrite :: Assertion
test_newToOriginal_deleteRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 2)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 3)
    assertEqual (Right $ SP 1 5) (f $ SP 1 4)
    assertEqual (Right $ SP 1 6) (f $ SP 1 5)
    assertEqual (Right $ SP 2 2) (f $ SP 2 2)
  where f = newToOriginalPos deleteRewrite

test_originalToNew_deleteRewrite :: Assertion
test_originalToNew_deleteRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 3)) (f $ SP 1 2)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 3)) (f $ SP 1 3)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 3)) (f $ SP 1 4)
    assertEqual (Right $ SP 1 4) (f $ SP 1 5)
    assertEqual (Right $ SP 2 2) (f $ SP 2 2)
  where f = originalToNewPos deleteRewrite

-- rewrite 1:2-1:4 --> 1:2-1:3
deleteRewrite :: SourceDiffs
deleteRewrite = Map.fromList [(SP 1 2, SourceDiffData (SP 1 4) (DSP 0 1 1))]

--------------------
---- Delete line

test_newToOriginal_deleteLineRewrite :: Assertion
test_newToOriginal_deleteLineRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 2 1)) (f $ SP 1 2)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 2 1)) (f $ SP 1 3)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 2 1)) (f $ SP 1 4)
    assertEqual (Right $ SP 2 2) (f $ SP 1 5)
    assertEqual (Right $ SP 3 2) (f $ SP 2 2)
  where f = newToOriginalPos deleteLineRewrite

test_originalToNew_deleteLineRewrite :: Assertion
test_originalToNew_deleteLineRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 2)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 1 666)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 4)) (f $ SP 2 1)
    assertEqual (Right $ SP 1 5) (f $ SP 2 2)
    assertEqual (Right $ SP 2 2) (f $ SP 3 2)
  where f = originalToNewPos deleteLineRewrite

-- rewrite 1:2-2:1 --> 1:2-1:4
deleteLineRewrite :: SourceDiffs
deleteLineRewrite = Map.fromList [(SP 1 2, SourceDiffData (SP 2 1) (DSP 0 2 2))]

-------------------------
---- Multiple additions

test_newToOriginal_multiAddRewrite :: Assertion
test_newToOriginal_multiAddRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 2)) (f $ SP 1 2)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 2)) (f $ SP 1 66)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 2)) (f $ SP 2 1)
    assertEqual (Right $ SP 1 3) (f $ SP 2 2)
    assertEqual (Right $ SP 2 10) (f $ SP 3 10)
    assertEqual (Left $ SourceRange (SP 3 1) (SP 3 1)) (f $ SP 4 1)
    assertEqual (Left $ SourceRange (SP 3 1) (SP 3 1)) (f $ SP 4 3)
    assertEqual (Right $ SP 3 2) (f $ SP 4 4)
  where f = newToOriginalPos multiAddRewrite

test_originalToNew_multiAddRewrite :: Assertion
test_originalToNew_multiAddRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 2 1)) (f $ SP 1 2)
    assertEqual (Right $ SP 2 2) (f $ SP 1 3)
    assertEqual (Right $ SP 3 10) (f $ SP 2 10)
    assertEqual (Left $ SourceRange (SP 4 1) (SP 4 3)) (f $ SP 3 1)
    assertEqual (Right $ SP 4 4) (f $ SP 3 2)
  where f = originalToNewPos multiAddRewrite

-- rewrite 1:2-1:2 --> 1:2-2:1, 3:1-3:1 -> 3:1-3:3 
multiAddRewrite :: SourceDiffs
multiAddRewrite = Map.fromList
  [ (SP 1 2, SourceDiffData (SP 1 2) (DSP 1 (-1) 2))
  , (SP 3 1, SourceDiffData (SP 3 1) (DSP 0 2 3))
  ]

-------------------------
---- Multiple deletions

test_newToOriginal_multiDeleteRewrite :: Assertion
test_newToOriginal_multiDeleteRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 2 1)) (f $ SP 1 2)
    assertEqual (Right $ SP 2 2) (f $ SP 1 3)
    assertEqual (Left $ SourceRange (SP 3 1) (SP 3 3)) (f $ SP 2 1)
    assertEqual (Right $ SP 3 4) (f $ SP 2 2)
    assertEqual (Right $ SP 4 2) (f $ SP 3 2)
  where f = newToOriginalPos multiDeleteRewrite

test_originalToNew_multiDeleteRewrite :: Assertion
test_originalToNew_multiDeleteRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 2)) (f $ SP 1 2)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 2)) (f $ SP 1 666)
    assertEqual (Left $ SourceRange (SP 1 2) (SP 1 2)) (f $ SP 2 1)
    assertEqual (Right $ SP 1 3) (f $ SP 2 2)
    assertEqual (Left $ SourceRange (SP 2 1) (SP 2 1)) (f $ SP 3 1)
    assertEqual (Left $ SourceRange (SP 2 1) (SP 2 1)) (f $ SP 3 2)
    assertEqual (Left $ SourceRange (SP 2 1) (SP 2 1)) (f $ SP 3 3)
    assertEqual (Right $ SP 2 2) (f $ SP 3 4)
    assertEqual (Right $ SP 3 4) (f $ SP 4 4)
  where f = originalToNewPos multiDeleteRewrite

-- rewrite 1:2-2:1 --> 1:2-1:2, 3:1-3:3 -> 3:1-3:1 
multiDeleteRewrite :: SourceDiffs
multiDeleteRewrite = Map.fromList
  [ (SP 1 2, SourceDiffData (SP 2 1) (DSP 0 0 2))
  , (SP 3 1, SourceDiffData (SP 3 3) (DSP 0 0 3))
  ]
