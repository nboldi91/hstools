{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.HsTools.LspServer.Utils where

import Control.Concurrent.MVar
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Text as T
import qualified Data.Vector as V
import Database.PostgreSQL.Simple (Connection)
import qualified Language.LSP.Types as LSP

import Language.Haskell.HsTools.FileLines
import Language.Haskell.HsTools.SourceDiffs
import Language.Haskell.HsTools.SourcePosition

textDocChangeToSD :: LSP.TextDocumentContentChangeEvent -> Maybe (Rewrite orig)
textDocChangeToSD (LSP.TextDocumentContentChangeEvent (Just (LSP.Range start end)) _ text)
  = Just $ Rewrite (Range (posToSP start) (posToSP end)) (T.unpack text)
textDocChangeToSD _ = Nothing

posToSP :: LSP.Position -> SP orig
posToSP (LSP.Position line char) = SP (fromIntegral line + 1) (fromIntegral char + 1)

spToPos :: SP orig -> LSP.Position
spToPos (SP line char) = LSP.Position (fromIntegral line - 1) (fromIntegral char - 1)

rangeToLSP :: Range orig -> LSP.Range
rangeToLSP (Range start end) = LSP.Range (spToPos start) (spToPos end)

nothingIfEmpty :: String -> Maybe String
nothingIfEmpty "" = Nothing
nothingIfEmpty str = Just str

instance Show Connection where
  show _ = "<Connection>"

createChangeFileStates :: [(FilePath, SourceDiffs orig mod)] -> A.Value
createChangeFileStates states = A.Object $ A.fromList [ ("result", payload )]
  where
    payload = A.Array $ V.fromList $ map diffToStates states
    diffToStates (fp, diff) = A.Object $ A.fromList [("filePath", A.String $ T.pack fp), ("state", toState diff)]
    toState diff = A.String $ if isEmptyDiffs diff then "fresh" else "edited"

changeFileStatesMethod = LSP.SCustomMethod "ChangeFileStates"

modifyMVarPure :: (a -> a) -> MVar a -> IO ()
modifyMVarPure f mv = modifyMVar_ mv (return . f)
