{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.HsTools.Database where

import Control.Monad
import Data.Char (toLower)
import Data.Maybe
import Data.Time.Clock
import Data.String

import Database.PostgreSQL.Simple

data LoadingState = NotLoaded | SourceSaved | NamesLoaded | TypesLoaded
    deriving (Show, Enum, Eq, Ord)

getCompiledTime :: Connection -> FilePath -> IO (Maybe UTCTime)
getCompiledTime conn filePath = fmap (fmap head . listToMaybe) $ query conn "SELECT compiledTime FROM modules WHERE filePath = ?" (Only filePath)

getModuleIdLoadingState :: Connection -> FilePath -> IO (Maybe (Int, LoadingState))
getModuleIdLoadingState conn filePath = do
  res <- query conn "SELECT moduleId, loadingState FROM modules WHERE filePath = ?" (Only filePath)
  return $ case res of
    [(moduleId, loadingState)] -> Just (moduleId, toEnum loadingState)
    [] -> Nothing
    _ -> error "getModuleIdLoadingState: filePath should be unique"

updateLoadingState :: Connection -> Int -> LoadingState -> IO ()
updateLoadingState conn moduleId newLoadingState =
  void $ execute conn
    "UPDATE modules SET loadingState = ? WHERE moduleId = ?"
    (fromEnum newLoadingState, moduleId)

updateFileDiffs :: Connection -> FilePath -> UTCTime -> Maybe String -> IO ()
updateFileDiffs conn filePath modifiedTime serializedDiff = void $
  execute conn "UPDATE modules SET modifiedTime = ?, modifiedFileDiffs = ? WHERE filePath = ?" (modifiedTime, serializedDiff, filePath)

getAllModifiedFileDiffs :: Connection -> IO [(String, Maybe String)]
getAllModifiedFileDiffs conn = query_ conn "SELECT filePath, modifiedFileDiffs FROM modules"

getCompiledSource :: Connection -> FilePath -> IO (Maybe String)
getCompiledSource conn filePath
  = fmap (fmap head . listToMaybe) $ query conn "SELECT compiledSource FROM modules WHERE filePath = ? AND compiledSource IS NOT NULL" (Only filePath)

getModifiedFileDiffs :: Connection -> FilePath -> IO (Maybe String)
getModifiedFileDiffs conn filePath
  = fmap (fmap head . listToMaybe) $ query conn "SELECT modifiedFileDiffs FROM modules WHERE filePath = ? AND modifiedFileDiffs IS NOT NULL" (Only filePath)

getCompiledSourceAndModifiedFileDiffs :: Connection -> FilePath -> IO (Maybe (String, String))
getCompiledSourceAndModifiedFileDiffs conn filePath
  = fmap listToMaybe $ query conn "SELECT compiledSource, modifiedFileDiffs FROM modules WHERE filePath = ? AND compiledSource IS NOT NULL AND modifiedFileDiffs IS NOT NULL" (Only filePath)

insertModule :: Connection -> FilePath -> UTCTime -> String -> String -> String -> IO Int
insertModule conn fullPath roundedModificationTime moduleName unitId content
  = head . head <$> query conn "INSERT INTO modules (filePath, compiledTime, moduleName, unitId, loadingState, compiledSource, modifiedTime, modifiedFileDiffs) VALUES (?, ?, ?, ?, 0, ?, null, null) RETURNING moduleId"
      (fullPath, roundedModificationTime, moduleName, unitId, content)

getAstNodes :: Connection -> Int -> IO [(Int, Int, Int, Int, Int)]
getAstNodes conn moduleId = query conn "SELECT startRow, startColumn, endRow, endColumn, astId FROM ast WHERE module = ?" (Only moduleId)

persistAst :: Connection -> [(Int, Int, Int, Int, Int)] -> IO [Int]
persistAst conn asts = fmap head <$> returning conn "INSERT INTO ast (module, startRow, startColumn, endRow, endColumn) VALUES (?, ?, ?, ?, ?) RETURNING astId" asts

persistName :: Connection -> [(Int, Int, String, Maybe Int, Bool)] -> IO ()
persistName conn names = void $ executeMany conn "INSERT INTO names (module, astNode, name, namespace, isDefined) VALUES (?, ?, ?, ?, ?)" names

persistTypes :: Connection -> [(Int, Int, String)] -> IO ()
persistTypes conn types = void $ executeMany conn "INSERT INTO types (module, astNode, type) VALUES (?, ?, ?)" types

persistTHRange' :: Connection -> Int -> Int -> IO ()
persistTHRange' conn mod astNode = void $ execute conn "INSERT INTO thRanges (module, astNode) VALUES (?, ?)" (mod, astNode)

getTHRanges :: Connection -> Int -> IO [(Int, Int, Int, Int)]
getTHRanges conn moduleId
  = query conn "SELECT startRow, startColumn, endRow, endColumn \
                  \FROM thRanges t JOIN ast a ON t.astNode = a.astId \
                  \WHERE a.module = ?" (Only moduleId)

getAstId :: Connection -> Int -> Int -> Int -> Int -> Int -> IO Int
getAstId conn moduleId startRow startCol endRow endCol
  = head . head <$>
      query conn "SELECT astId FROM ast WHERE module = ? AND startRow = ? AND startColumn = ? AND endRow = ? AND endColumn = ?"
        (moduleId, startRow, startCol, endRow, endCol)

insertAstId :: Connection -> Int -> Int -> Int -> Int -> Int -> IO Int
insertAstId conn moduleId startRow startCol endRow endCol
  = head . head <$>
      returning conn "INSERT INTO ast (module, startRow, startColumn, endRow, endColumn) VALUES (?, ?, ?, ?, ?) RETURNING astId"
        [(moduleId, startRow, startCol, endRow, endCol)]

getMatchingNames :: Connection -> FilePath -> Int -> Int -> Maybe Bool -> IO [(String, Int, Int, Int, Int)]
getMatchingNames conn file row col onlyDefinitions
  = query conn
      (fromString $ 
        "SELECT dm.filePath, d.startRow, d.startColumn, d.endRow, d.endColumn \
          \FROM ast AS n JOIN names nn ON nn.astNode = n.astId \
            \JOIN names AS dn ON nn.name = dn.name AND nn.namespace = dn.namespace \
            \JOIN ast AS d ON d.astId = dn.astNode \
            \JOIN modules nm ON n.module = nm.moduleId \
            \JOIN modules dm ON d.module = dm.moduleId \
          \WHERE nm.filePath = ? AND n.startRow <= ? AND n.endRow >= ? AND n.startColumn <= ? AND n.endColumn >= ? "
          ++ (maybe "" (("AND dn.isDefined = " ++) . show) onlyDefinitions))
      (file, row, row, col, col)

getHoverInfo :: Connection -> FilePath -> Int -> Int -> IO [(Maybe String, Bool, String, Int, Int, Int, Int)]
getHoverInfo conn file row col =
  query
    conn
    "SELECT tn.type, nn.isDefined, nn.name, n.startRow, n.startColumn, n.endRow, n.endColumn \
        \FROM ast n \
        \JOIN names nn ON nn.astNode = n.astId \
        \JOIN modules nm ON n.module = nm.moduleId \
        \LEFT JOIN types tn ON n.astId = tn.astNode \
        \WHERE nm.filePath = ? AND n.startRow <= ? AND n.endRow >= ? AND n.startColumn <= ? AND n.endColumn >= ?"
    (file, row, row, col, col)

listenToModuleClean :: Connection -> IO ()
listenToModuleClean conn = void $ execute_ conn "LISTEN module_clean"

cleanDB :: Connection -> IO ()
cleanDB conn = void $ do
  execute_ conn "DROP TABLE modules, ast, names, types, thRanges CASCADE"
  execute_ conn "DROP TRIGGER modulesNotifyChange"

cleanModuleFromDB :: Connection -> FilePath -> IO ()
cleanModuleFromDB conn filePath = do
    moduleIds <- query conn "SELECT moduleId FROM modules WHERE filePath = ?" [filePath]
    forM_ (moduleIds :: [[Int]]) $ \[moduleId] -> do
      void $ execute conn "DELETE FROM names WHERE module = ?" [moduleId]
      void $ execute conn "DELETE FROM types WHERE module = ?" [moduleId]
      void $ execute conn "DELETE FROM thRanges WHERE module = ?" [moduleId]
      void $ execute conn "DELETE FROM ast WHERE module = ?" [moduleId]
      void $ execute conn "DELETE FROM modules WHERE moduleId = ?" [moduleId]

initializeTables :: Connection -> IO ()
initializeTables conn = do 
    tables <- query_ conn "SELECT tablename FROM pg_tables"
    indices <- query_ conn "SELECT indexname FROM pg_indexes"
    let allExistingDefs = map (toLowerCase . head) $ tables ++ indices
    mapM_ (execute_ conn) (map snd . filter ((`notElem` allExistingDefs) . toLowerCase . fst) $ tableDefs)
  where
    toLowerCase = map toLower

    tableDefs :: [(String, Query)]
    tableDefs =
      [ ("modules", "CREATE TABLE modules \
          \(moduleId SERIAL PRIMARY KEY\
          \,filePath TEXT UNIQUE NOT NULL\
          \,unitId TEXT NOT NULL\
          \,moduleName TEXT NOT NULL\
          \,compiledTime TIMESTAMP WITH TIME ZONE NOT NULL\
          \,loadingState INT NOT NULL\
          \,compiledSource TEXT NOT NULL\
          \,modifiedTime TIMESTAMP WITH TIME ZONE\
          \,modifiedFileDiffs TEXT\
          \);"
      )
      , ("ast", "CREATE TABLE ast \
          \(module INT NOT NULL\
          \,CONSTRAINT fk_ast_module FOREIGN KEY(module) REFERENCES modules(moduleId)\
          \,astId SERIAL PRIMARY KEY\
          \,startRow INT NOT NULL\
          \,startColumn INT NOT NULL\
          \,endRow INT NOT NULL\
          \,endColumn INT NOT NULL\
          \);"
      )
      , ("names", "CREATE TABLE names \
          \(module INT NOT NULL\
          \,astNode INT NOT NULL\
          \,CONSTRAINT fk_name_ast FOREIGN KEY(astNode) REFERENCES ast(astId)\
          \,isDefined BOOL NOT NULL\
          \,name TEXT NOT NULL\
          \,namespace INT\
          \);"
      )
      , ("types", "CREATE TABLE types \
          \(module INT NOT NULL\
          \,astNode INT NOT NULL\
          \,CONSTRAINT fk_type_ast FOREIGN KEY(astNode) REFERENCES ast(astId)\
          \,type TEXT NOT NULL\
          \);"
      )
      , ("thRanges", "CREATE TABLE thRanges \
          \(module INT NOT NULL\
          \,astNode INT NOT NULL\
          \,CONSTRAINT fk_type_ast FOREIGN KEY(astNode) REFERENCES ast(astId)\
          \);"
      )
      , ("notifyModulesFunction", "CREATE OR REPLACE FUNCTION notifyModulesFunction() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN IF NEW.modifiedFileDiffs IS NULL THEN PERFORM pg_notify('module_clean', NEW.filePath); END IF; RETURN NEW; END; $$")
      , ("notifyModulesTrigger", "CREATE OR REPLACE TRIGGER notifyModulesTrigger AFTER INSERT OR UPDATE ON modules FOR EACH ROW EXECUTE FUNCTION notifyModulesFunction()")
      ]
