{-# OPTIONS_GHC -F -pgmF htfpp #-}
import Test.Framework

import {-@ HTF_TESTS @-} PluginTests

main :: IO ()
main = htfMain htf_importedTests
