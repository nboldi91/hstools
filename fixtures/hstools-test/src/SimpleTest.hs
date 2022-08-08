{-# LANGUAGE TemplateHaskell #-}
module SimpleTest where

import Language.Haskell.TH.Syntax (Dec, Q)

generateDef :: Q [Dec]
generateDef = [d| a = "a" |]
