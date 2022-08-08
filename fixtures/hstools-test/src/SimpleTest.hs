{-# LANGUAGE TemplateHaskell #-}
module SimpleTest where

import Language.Haskell.TH.Syntax (Dec, Q)

generateDef :: String -> Q [Dec]
generateDef str = [d| a = str |]

a :: String
a = "a"