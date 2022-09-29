{-# OPTIONS_GHC -F -pgmF htfpp #-}
import Test.Framework

import {-@ HTF_TESTS @-} ConvertPositionTests
import {-@ HTF_TESTS @-} AddRewriteTests

main :: IO ()
main = htfMain htf_importedTests
