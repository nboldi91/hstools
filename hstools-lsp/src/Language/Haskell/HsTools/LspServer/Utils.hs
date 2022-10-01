{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HsTools.LspServer.Utils where

import Control.Concurrent.MVar
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Database.PostgreSQL.Simple (Connection)
import Language.LSP.Types as LSP

import Language.Haskell.HsTools.LinesDiff

textDocChangeToSD :: LSP.TextDocumentContentChangeEvent -> Maybe SourceRewrite
textDocChangeToSD (LSP.TextDocumentContentChangeEvent (Just (LSP.Range start end)) _ text)
  = Just $ SourceRewrite st (posToSP end) (T.unpack text)
  where st = posToSP start
textDocChangeToSD _ = Nothing

posToSP :: LSP.Position -> SP
posToSP (LSP.Position line char) = SP (fromIntegral line + 1) (fromIntegral char + 1)

spToPos :: SP -> LSP.Position
spToPos (SP line char) = LSP.Position (fromIntegral line - 1) (fromIntegral char - 1)

rangeToLSP :: SourceRange -> LSP.Range
rangeToLSP (SourceRange start end) = LSP.Range (spToPos start) (spToPos end)

lineToLoc :: SourceDiffs -> (String, Int, Int, Int, Int) -> Maybe LSP.Location
lineToLoc rewrites (file, startLine, startCol, endLine, endCol)
  = fmap (LSP.Location (filePathToUri file) . rangeToLSP) 
      $ originalToNewRangeStrict rewrites 
      $ SourceRange (SP startLine startCol) (SP endLine endCol)

nothingIfEmpty :: String -> Maybe String
nothingIfEmpty "" = Nothing
nothingIfEmpty str = Just str

instance Show Connection where
  show _ = "<Connection>"

createChangeFileStates :: [(FilePath, SourceDiffs)] -> A.Value
createChangeFileStates states = A.Object $ A.fromList [ ("result", payload )]
  where
    payload = A.Array $ V.fromList $ map diffToStates states
    diffToStates (fp, diff) = A.Object $ A.fromList [("filePath", A.String $ T.pack fp), ("state", toState diff)]
    toState diff = A.String $ if Map.null diff then "fresh" else "edited"

changeFileStatesMethod = SCustomMethod "ChangeFileStates"

modifyMVarPure :: (a -> a) -> MVar a -> IO ()
modifyMVarPure f mv = modifyMVar_ mv (return . f)
