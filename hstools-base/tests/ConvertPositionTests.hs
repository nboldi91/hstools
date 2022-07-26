{-# OPTIONS_GHC -F -pgmF htfpp #-}
module ConvertPositionTests ( htf_thisModulesTests ) where

import qualified Data.Map as Map

import Test.Framework
import Test.HUnit.Base (Assertion)

import Language.Haskell.HsTools.SourcePosition
import Language.Haskell.HsTools.SourceDiffs
import Language.Haskell.HsTools.Database

data Orig
data Mod

-----------------------
---- Add characters 

test_newToOriginal_oneRewrite :: Assertion
test_newToOriginal_oneRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 2)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 3)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 4)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 5)
    assertEqual (Right $ SP 1 5) (f $ SP 1 6)
    assertEqual (Right $ SP 2 3) (f $ SP 2 3)
  where f = newToOriginalPos oneRewrite

test_originalToNew_oneRewrite :: Assertion
test_originalToNew_oneRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 1 5)) (f $ SP 1 2)
    assertEqual (Left $ Range (SP 1 2) (SP 1 5)) (f $ SP 1 3)
    assertEqual (Left $ Range (SP 1 2) (SP 1 5)) (f $ SP 1 4)
    assertEqual (Right $ SP 1 6) (f $ SP 1 5)
    assertEqual (Right $ SP 1 7) (f $ SP 1 6)
    assertEqual (Right $ SP 2 3) (f $ SP 2 3)
  where f = originalToNewPos oneRewrite

oneRewrite :: SourceDiffs Orig Mod
oneRewrite = deserializeSourceDiffs "1:2-1:4 -> 1:2-1:5"

-----------------
---- Add line 

test_newToOriginal_addLineRewrite :: Assertion
test_newToOriginal_addLineRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 2)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 3)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 4)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 2 1)
    assertEqual (Right $ SP 1 5) (f $ SP 2 2)
    assertEqual (Right $ SP 1 6) (f $ SP 2 3)
    assertEqual (Right $ SP 2 1) (f $ SP 3 1)
    assertEqual (Right $ SP 2 2) (f $ SP 3 2)
  where f = newToOriginalPos addLineRewrite

test_originalToNew_addLineRewrite :: Assertion
test_originalToNew_addLineRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 2 1)) (f $ SP 1 2)
    assertEqual (Left $ Range (SP 1 2) (SP 2 1)) (f $ SP 1 3)
    assertEqual (Left $ Range (SP 1 2) (SP 2 1)) (f $ SP 1 4)
    assertEqual (Right $ SP 2 2) (f $ SP 1 5)
    assertEqual (Right $ SP 3 1) (f $ SP 2 1)
  where f = originalToNewPos addLineRewrite

addLineRewrite :: SourceDiffs Orig Mod
addLineRewrite = deserializeSourceDiffs "1:2-1:4 -> 1:2-2:1"

--------------------------------
---- Add line and characters

test_newToOriginal_addLineCharRewrite :: Assertion
test_newToOriginal_addLineCharRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 2)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 3)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 4)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 2 1)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 2 3)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 2 4)
    assertEqual (Right $ SP 1 5) (f $ SP 2 5)
    assertEqual (Right $ SP 1 6) (f $ SP 2 6)
    assertEqual (Right $ SP 2 1) (f $ SP 3 1)
    assertEqual (Right $ SP 2 2) (f $ SP 3 2)
  where f = newToOriginalPos addLineCharRewrite

test_originalToNew_addLineCharRewrite :: Assertion
test_originalToNew_addLineCharRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 2 4)) (f $ SP 1 2)
    assertEqual (Left $ Range (SP 1 2) (SP 2 4)) (f $ SP 1 3)
    assertEqual (Left $ Range (SP 1 2) (SP 2 4)) (f $ SP 1 4)
    assertEqual (Right $ SP 2 5) (f $ SP 1 5)
    assertEqual (Right $ SP 2 6) (f $ SP 1 6)
    assertEqual (Right $ SP 3 1) (f $ SP 2 1)
    assertEqual (Right $ SP 3 2) (f $ SP 2 2)
  where f = originalToNewPos addLineCharRewrite

addLineCharRewrite :: SourceDiffs Orig Mod
addLineCharRewrite = deserializeSourceDiffs "1:2-1:4 -> 1:2-2:4"

--------------------
---- Delete range

test_newToOriginal_deleteRewrite :: Assertion
test_newToOriginal_deleteRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 2)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 3)
    assertEqual (Right $ SP 1 5) (f $ SP 1 4)
    assertEqual (Right $ SP 1 6) (f $ SP 1 5)
    assertEqual (Right $ SP 2 2) (f $ SP 2 2)
  where f = newToOriginalPos deleteRewrite

test_originalToNew_deleteRewrite :: Assertion
test_originalToNew_deleteRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 1 3)) (f $ SP 1 2)
    assertEqual (Left $ Range (SP 1 2) (SP 1 3)) (f $ SP 1 3)
    assertEqual (Left $ Range (SP 1 2) (SP 1 3)) (f $ SP 1 4)
    assertEqual (Right $ SP 1 4) (f $ SP 1 5)
    assertEqual (Right $ SP 2 2) (f $ SP 2 2)
  where f = originalToNewPos deleteRewrite

deleteRewrite :: SourceDiffs Orig Mod
deleteRewrite = deserializeSourceDiffs "1:2-1:4 -> 1:2-1:3"

--------------------
---- Delete line

test_newToOriginal_deleteLineRewrite :: Assertion
test_newToOriginal_deleteLineRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 2 1)) (f $ SP 1 2)
    assertEqual (Left $ Range (SP 1 2) (SP 2 1)) (f $ SP 1 3)
    assertEqual (Left $ Range (SP 1 2) (SP 2 1)) (f $ SP 1 4)
    assertEqual (Right $ SP 2 2) (f $ SP 1 5)
    assertEqual (Right $ SP 3 2) (f $ SP 2 2)
  where f = newToOriginalPos deleteLineRewrite

test_originalToNew_deleteLineRewrite :: Assertion
test_originalToNew_deleteLineRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 2)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 1 666)
    assertEqual (Left $ Range (SP 1 2) (SP 1 4)) (f $ SP 2 1)
    assertEqual (Right $ SP 1 5) (f $ SP 2 2)
    assertEqual (Right $ SP 2 2) (f $ SP 3 2)
  where f = originalToNewPos deleteLineRewrite

deleteLineRewrite :: SourceDiffs Orig Mod
deleteLineRewrite = deserializeSourceDiffs "1:2-2:1 -> 1:2-1:4"

-------------------------
---- Multiple additions

test_newToOriginal_multiAddRewrite :: Assertion
test_newToOriginal_multiAddRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 1 2)) (f $ SP 1 2)
    assertEqual (Left $ Range (SP 1 2) (SP 1 2)) (f $ SP 1 66)
    assertEqual (Left $ Range (SP 1 2) (SP 1 2)) (f $ SP 2 1)
    assertEqual (Right $ SP 1 3) (f $ SP 2 2)
    assertEqual (Right $ SP 2 10) (f $ SP 3 10)
    assertEqual (Left $ Range (SP 3 1) (SP 3 1)) (f $ SP 4 1)
    assertEqual (Left $ Range (SP 3 1) (SP 3 1)) (f $ SP 4 3)
    assertEqual (Right $ SP 3 2) (f $ SP 4 4)
  where f = newToOriginalPos multiAddRewrite

test_originalToNew_multiAddRewrite :: Assertion
test_originalToNew_multiAddRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 2 1)) (f $ SP 1 2)
    assertEqual (Right $ SP 2 2) (f $ SP 1 3)
    assertEqual (Right $ SP 3 10) (f $ SP 2 10)
    assertEqual (Left $ Range (SP 4 1) (SP 4 3)) (f $ SP 3 1)
    assertEqual (Right $ SP 4 4) (f $ SP 3 2)
  where f = originalToNewPos multiAddRewrite

multiAddRewrite :: SourceDiffs Orig Mod
multiAddRewrite = deserializeSourceDiffs "1:2-1:2 -> 1:2-2:1 , 3:1-3:1 -> 4:1-4:3"

-------------------------
---- Multiple deletions

test_newToOriginal_multiDeleteRewrite :: Assertion
test_newToOriginal_multiDeleteRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 2 1)) (f $ SP 1 2)
    assertEqual (Right $ SP 2 2) (f $ SP 1 3)
    assertEqual (Left $ Range (SP 3 1) (SP 3 3)) (f $ SP 2 1)
    assertEqual (Right $ SP 3 4) (f $ SP 2 2)
    assertEqual (Right $ SP 4 2) (f $ SP 3 2)
  where f = newToOriginalPos multiDeleteRewrite

test_originalToNew_multiDeleteRewrite :: Assertion
test_originalToNew_multiDeleteRewrite = do
    assertEqual (Right $ SP 1 1) (f $ SP 1 1)
    assertEqual (Left $ Range (SP 1 2) (SP 1 2)) (f $ SP 1 2)
    assertEqual (Left $ Range (SP 1 2) (SP 1 2)) (f $ SP 1 666)
    assertEqual (Left $ Range (SP 1 2) (SP 1 2)) (f $ SP 2 1)
    assertEqual (Right $ SP 1 3) (f $ SP 2 2)
    assertEqual (Left $ Range (SP 2 1) (SP 2 1)) (f $ SP 3 1)
    assertEqual (Left $ Range (SP 2 1) (SP 2 1)) (f $ SP 3 2)
    assertEqual (Left $ Range (SP 2 1) (SP 2 1)) (f $ SP 3 3)
    assertEqual (Right $ SP 2 2) (f $ SP 3 4)
    assertEqual (Right $ SP 3 4) (f $ SP 4 4)
  where f = originalToNewPos multiDeleteRewrite

multiDeleteRewrite :: SourceDiffs Orig Mod
multiDeleteRewrite = deserializeSourceDiffs "1:2-2:1 -> 1:2-1:2 , 3:1-3:3 -> 2:1-2:1 "
