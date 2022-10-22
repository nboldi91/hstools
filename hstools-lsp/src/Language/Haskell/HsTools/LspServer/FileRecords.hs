{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Language.Haskell.HsTools.LspServer.FileRecords where

import Control.Concurrent.MVar
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)

import Language.Haskell.HsTools.Database
import Language.Haskell.HsTools.LspServer.Utils
import Language.Haskell.HsTools.SourceDiffs
import Language.Haskell.HsTools.SourcePosition
import Language.Haskell.HsTools.FileLines
import Language.Haskell.HsTools.Utils

data FileRecord
  = FileRecord { frDiffs :: SourceDiffs Original Modified }
  | OpenFileRecord 
    { frDiffs :: SourceDiffs Original Modified
    , frCompiledContent :: FileLines
    , frCurrentContent :: FileLines
    }
  deriving (Show)

-- Nothing means it is not loaded yet from database
type FileRecords = MVar (Map.Map FilePath FileRecord)

data Original
data Modified

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
  = modifyMVar_ mv $ \frs -> updateRecord (Map.lookup fp frs) >>= return . maybe frs (\r -> Map.insert fp r frs)
  where
    updateRecord (Just (OpenFileRecord _diffs _compiledContent _currentContent)) = do
      modifiedDiffs <- getModifiedFileDiffs conn fp
      return $ Just $ FileRecord $ maybe emptyDiffs deserializeSourceDiffs modifiedDiffs
    updateRecord fileRecord = return fileRecord

markFileRecordsClean :: [FilePath] -> FileRecords -> IO ()
markFileRecordsClean files = modifyMVarPure
    (Map.mapWithKey (\fp fr -> if fp `elem` files then fr{frDiffs = emptyDiffs} else fr)
      . (`Map.union` (Map.fromList $ map (, FileRecord emptyDiffs) files)))

updateSavedFileRecords :: FilePath -> [Rewrite Modified] -> FileRecords -> IO ()
updateSavedFileRecords fp newDiffs = modifyMVarPure $ \fr -> Map.adjust updateDiffs fp fr
  where
    updateDiffs (OpenFileRecord diffs compiledContent currentContent) =
      let (currentContent', diffs') = foldr (addExtraChange compiledContent) (currentContent, diffs) newDiffs
      in OpenFileRecord diffs' compiledContent currentContent'
    updateDiffs fr = fr

replaceSourceDiffs :: FilePath -> SourceDiffs Original Modified -> FileRecords -> IO ()
replaceSourceDiffs fp diffs = modifyMVarPure $ Map.adjust (\fr -> fr{frDiffs = diffs}) fp

isFileOpen :: FilePath -> FileRecords -> IO Bool
isFileOpen fp frsMVar = do
  frs <- readMVar frsMVar
  return $ case Map.lookup fp frs of
    Just OpenFileRecord{} -> True
    _ -> False

checkIfFilesHaveBeenChanged :: Connection -> IO [(String, SourceDiffs Original Modified)]
checkIfFilesHaveBeenChanged conn = do 
  diffs <- getAllModifiedFileDiffs conn
  forM diffs $ \(filePath, diff, modifiedTime) -> do
    modificationTime <- getFileModificationTime filePath
    case (modifiedTime, modificationTime) of
      (Just recTime, Just modTime) | recTime < modTime -> updateDiffs filePath modTime
      (Nothing, Just modTime) -> updateDiffs filePath modTime
      _ -> return (filePath, maybe emptyDiffs deserializeSourceDiffs diff)
  where
    updateDiffs filePath modificationTime = do
      fileContent <- readFileContent filePath
      compiledSource <- getCompiledSource conn filePath
      case (fileContent, compiledSource) of
        (Just fc, Just cs) -> do
          let newDiff = createSourceDiffs (SP 1 1) (SP 1 1) cs fc
          updateFileDiffs conn filePath modificationTime (Just $ serializeSourceDiffs newDiff)
          return (filePath, newDiff)
        _ -> return (filePath, emptyDiffs)
