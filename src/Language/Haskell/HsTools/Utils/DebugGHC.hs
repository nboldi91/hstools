{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HsTools.Utils.DebugGHC (showDebug) where
  
import Data.Data
import qualified Data.Text as T

showDebug :: Data a => a -> T.Text
showDebug = showDebugNode 0 . showDebug'

showDebugNode :: Int -> DebugNode -> T.Text
showDebugNode indent node
  = T.replicate indent "  " `T.append` T.pack (ctor node) `T.append` "\n"
      `T.append` (T.concat $ map (showDebugNode (indent + 1)) (children node))

data DebugNode = DebugNode { ctor :: String, children :: [DebugNode] }

showDebug' :: Data a => a -> DebugNode
showDebug' v = DebugNode (showConstr $ toConstr v) (gmapQ showDebug' v)
