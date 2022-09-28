{-# OPTIONS_GHC -F -pgmF htfpp #-}
import Test.Framework

import {-@ HTF_TESTS @-} ConvertPositionTests

main :: IO ()
main = htfMain htf_importedTests
