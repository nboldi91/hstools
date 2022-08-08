{-# LANGUAGE TemplateHaskell #-}
module SimpleTest2 where

import SimpleTest

foo :: String
foo = "foo"

$(generateDef a)

bar :: String
bar = "bar"