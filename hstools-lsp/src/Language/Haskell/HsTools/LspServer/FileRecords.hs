{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Language.Haskell.HsTools.LspServer.FileRecords where

import Control.Monad
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time.Clock

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

-- Nothing means it is not loaded yet from database
type FileRecords = MVar (Map.Map FilePath FileRecord)

data Original
data Modified

recordFileOpened :: DBMonad m => FilePath -> T.Text -> SourceDiffs Original Modified -> FileRecords -> m ()
recordFileOpened fp content diffs mv = do
    runInIO <- askRunInIO
    liftIO $ modifyMVar_ mv $ \frs -> runInIO $ updateRecord (Map.lookup fp frs) >>= (\r -> return $ Map.alter (const r) fp frs)
  where
    updateRecord fileRecord@(Just OpenFileRecord{}) = return fileRecord
    updateRecord _ =
      fmap (\src -> OpenFileRecord diffs (toFileLines src) contentLines) <$> getCompiledSource fp
    contentLines = toFileLines $ T.unpack content

recordFileClosed :: DBMonad m => FilePath -> FileRecords -> m ()
recordFileClosed fp fr = do
    runInIO <- askRunInIO
    liftIO $ modifyMVar_ fr $ \frs -> runInIO $ updateRecord (Map.lookup fp frs) >>= return . maybe frs (\r -> Map.insert fp r frs)
  where
    updateRecord (Just (OpenFileRecord _diffs _compiledContent _currentContent)) = do
      modifiedDiffs <- getModifiedFileDiffs fp
      return $ Just $ FileRecord $ maybe emptyDiffs deserializeSourceDiffs modifiedDiffs
    updateRecord fileRecord = return fileRecord

markFileRecordsClean :: [FilePath] -> FileRecords -> IO ()
markFileRecordsClean files = modifyMVarPure
    (Map.mapWithKey (\fp fr -> if fp `elem` files then fr{frDiffs = emptyDiffs} else fr)
      . (`Map.union` (Map.fromList $ map (, FileRecord emptyDiffs) files)))

updateSavedFileRecords :: FilePath -> [Rewrite Modified] -> FileRecords -> IO ()
updateSavedFileRecords fp newDiffs = modifyMVarPure $ Map.adjust updateDiffs fp
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

checkIfFileHaveBeenChanged :: DBMonad m => FilePath -> m (SourceDiffs Original Modified)
checkIfFileHaveBeenChanged fp = do
  (diffs, time) <- getModifiedTimeAndFileDiffs fp
  snd <$> checkFileHaveBeenChanged (fp, diffs, time)

-- | The modification time of a file if it is newer then we last checked, Nothing if the record has the latest version
getFileLaterModificationTime :: DBMonad m => (FilePath, Maybe String, Maybe UTCTime) -> m (Maybe UTCTime)
getFileLaterModificationTime (filePath, _, modifiedTime) = do
  modificationTime <- liftIO $ getFileModificationTime filePath
  return $ case (modifiedTime, modificationTime) of
    (Just recTime, Just modTime) | recTime < modTime -> Just modTime
    (Nothing, Just modTime) -> Just modTime
    _ -> Nothing

checkFileHaveBeenChanged :: DBMonad m => (FilePath, Maybe String, Maybe UTCTime) -> m (FilePath, SourceDiffs Original Modified)
checkFileHaveBeenChanged r@(filePath, diff, _) = do
  modificationTime <- getFileLaterModificationTime r
  case modificationTime of
    Just modTime -> updateDiffs filePath modTime
    Nothing -> return (filePath, maybe emptyDiffs deserializeSourceDiffs diff)
  where
    updateDiffs filePath modificationTime = do
      fileContent <- liftIO $ readFileContent filePath
      compiledSource <- getCompiledSource filePath
      case (fileContent, compiledSource) of
        (Just fc, Just cs) -> do
          let newDiff = createSourceDiffs (SP 1 1) (SP 1 1) cs fc
          updateFileDiffs filePath modificationTime (Just $ serializeSourceDiffs newDiff)
          return (filePath, newDiff)
        _ -> return (filePath, emptyDiffs)
