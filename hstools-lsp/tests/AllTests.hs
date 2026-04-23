{-# OPTIONS_GHC -F -pgmF htfpp #-}
import Test.Framework

import {-@ HTF_TESTS @-} LspTests
import {-@ HTF_TESTS @-} IntegrationTests

main :: IO ()
main = htfMain htf_importedTests
