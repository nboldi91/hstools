module Language.Haskell.HsTools.LspServer.FileRecords where

import Control.Concurrent.MVar
import qualified Data.Map as Map
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)

import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.LspServer.Utils
import Language.Haskell.HsTools.LinesDiff

data FileRecord
  = FileRecord { frDiffs :: SourceDiffs }
  | OpenFileRecord 
    { frDiffs :: SourceDiffs
    , frCompiledContent :: FileLines
    , frCurrentContent :: FileLines
    }
  deriving (Show)

-- Nothing means it is not loaded yet from database
type FileRecords = MVar (Map.Map FilePath FileRecord)

recordFileOpened :: Connection -> FilePath -> T.Text -> FileRecords -> IO ()
recordFileOpened conn fp content mv
  = modifyMVar_ mv $ \frs -> updateRecord (Map.lookup fp frs) >>= \r -> return $ Map.alter (const r) fp frs
  where
    updateRecord Nothing =
      fmap (\(src, diffs) -> OpenFileRecord (deserializeSourceDiffs diffs) (toFileLines src) contentLines) 
        <$> getCompiledSourceAndModifiedFileDiffs conn fp
    updateRecord (Just (FileRecord diffs)) = do
      compiledSource <- getCompiledSource conn fp
      return $ fmap (\src -> OpenFileRecord diffs (toFileLines src) contentLines) compiledSource
    updateRecord fileRecord = return fileRecord
    contentLines = toFileLines $ T.unpack content

recordFileClosed :: Connection -> FilePath -> FileRecords -> IO ()
recordFileClosed conn fp mv
  = modifyMVar_ mv $ \frs -> updateRecord (Map.lookup fp frs) >>= \r -> return $ Map.insert fp r frs
  where
    updateRecord (Just (OpenFileRecord _diffs _compiledContent _currentContent)) = do
      modifiedDiffs <- getModifiedFileDiffs conn fp
      return $ FileRecord $ maybe Map.empty (Map.fromAscList . map read . lines) modifiedDiffs
    updateRecord _ = error $ "recordFileClosed: file " ++ fp ++ " should have been on record"

markFileRecordsClean :: [FilePath] -> FileRecords -> IO ()
markFileRecordsClean files
  = modifyMVarPure $ Map.mapWithKey (\fp fr -> if fp `elem` files then fr{frDiffs = Map.empty} else fr)

updateSavedFileRecords :: FilePath -> [SourceRewrite] -> FileRecords -> IO ()
updateSavedFileRecords fp newDiffs = modifyMVarPure $ Map.adjust updateDiffs fp
  where
    updateDiffs (OpenFileRecord diffs compiledContent currentContent) =
      let (currentContent', diffs') = foldr (addExtraChange compiledContent) (currentContent, diffs) newDiffs
      in OpenFileRecord diffs' compiledContent currentContent'
    updateDiffs fr = fr

replaceSourceDiffs :: FilePath -> SourceDiffs -> FileRecords -> IO ()
replaceSourceDiffs fp diffs = modifyMVarPure $ Map.adjust (\fr -> fr{frDiffs = diffs}) fp

isFileOpen :: FilePath -> FileRecords -> IO Bool
isFileOpen fp frsMVar = do
  frs <- readMVar frsMVar
  return $ case Map.lookup fp frs of
    Just OpenFileRecord{} -> True
    _ -> False
