{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Haskell.HsTools.Database where

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Time.Clock
import Data.String
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Database.PostgreSQL.Simple

data LoadingState = NotLoaded | SourceSaved | NamesLoaded | TypesLoaded
    deriving (Show, Enum, Eq, Ord)

data Namespace = TyVarNS | TyConNS | DataConNS | ValNS | VarNS | ModuleNS deriving (Show, Enum, Eq, Ord)

data DefinitionKind
  = DefModule | DefSignature | DefInstance | DefPatternSynonym | DefClassOpSignature | DefValue | DefTypeClass
  | DefParameter | DefTypeDecl | DefConstructor | DefCtorArg
  deriving (Show, Eq, Ord, Enum)

isSignatureDef :: DefinitionKind -> Bool
isSignatureDef DefSignature = True
isSignatureDef DefClassOpSignature = True
isSignatureDef _ = False

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

getAllModifiedFileDiffs :: Connection -> IO [(FilePath, Maybe String, Maybe UTCTime)]
getAllModifiedFileDiffs conn = query_ conn "SELECT filePath, modifiedFileDiffs, modifiedTime FROM modules"

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

getAllNames :: Connection -> IO [(Int, Int, String, Maybe String, Bool)]
getAllNames conn = query_ conn "SELECT startRow, startColumn, name, type, isDefined FROM ast AS n JOIN names nn ON nn.astNode = n.astId LEFT JOIN types tt ON tt.typedName = nn.name AND tt.typeNamespace = nn.namespace ORDER BY startRow, startColumn"

persistAst :: Connection -> [(Int, Int, Int, Int, Int)] -> IO [Int]
persistAst conn asts = fmap head <$> returning conn "INSERT INTO ast (module, startRow, startColumn, endRow, endColumn) VALUES (?, ?, ?, ?, ?) RETURNING astId" asts

persistDefinitions :: Connection -> [(Int, Int, DefinitionKind)] -> IO [Int]
persistDefinitions conn definitions = fmap head <$> returning conn "INSERT INTO definitions (module, astNode, definitionKind) VALUES (?, ?, ?) RETURNING definitionId" (map (\(a,b,c) -> (a,b, fromEnum c)) definitions)

persistName :: Connection -> [(Int, Int, String, Maybe Int, Bool, Maybe Int, Maybe Int, Maybe Int, Maybe Int)] -> IO ()
persistName conn names = void $ executeMany conn "INSERT INTO names (module, astNode, name, namespace, isDefined, namedDefinitionRow, namedDefinitionColumn, namedDefinitionEndRow, namedDefinitionEndColumn) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)" names

persistTypes :: Connection -> [(String, Maybe Int, String)] -> IO ()
persistTypes conn types = void $ executeMany conn "INSERT INTO types (typedName, typeNamespace, type) VALUES (?, ?, ?)" types

persistTHRange' :: Connection -> Int -> Int -> IO ()
persistTHRange' conn mod astNode = void $ execute conn "INSERT INTO thRanges (module, astNode) VALUES (?, ?)" (mod, astNode)

persistComments :: Connection -> [(Int, Int, String)] -> IO ()
persistComments conn comments = void $ executeMany conn "INSERT INTO comments (module, targetDefinition, commentText) VALUES (?, ?, ?)" comments

getAllComments :: Connection -> IO [(Int, Int, String)]
getAllComments conn = query_ conn "SELECT startRow, startColumn, commentText FROM ast a JOIN comments c ON c.targetDefinition = a.astId ORDER BY startRow, startColumn"

getAllDefinitions :: Connection -> IO [(DefinitionKind, Maybe String, Int, Int, Int, Int)]
getAllDefinitions conn = fmap (\(k,n,a,b,c,d) -> (toEnum k,n,a,b,c,d)) <$> query_ conn "SELECT definitionKind, name, startRow, startColumn, endRow, endColumn FROM definitions d JOIN ast a ON d.astNode = a.astId LEFT JOIN names n ON a.startRow = n.namedDefinitionRow AND a.startColumn = n.namedDefinitionColumn AND a.endRow = n.namedDefinitionEndRow AND a.endColumn = n.namedDefinitionEndColumn ORDER BY startRow, startColumn"

getTHRanges :: Connection -> Int -> IO [(Int, Int, Int, Int)]
getTHRanges conn moduleId
  = query conn [sql|
    SELECT startRow, startColumn, endRow, endColumn
      FROM thRanges t JOIN ast a ON t.astNode = a.astId
      WHERE a.module = ?
    |] (Only moduleId)

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
      ([sql|
        SELECT dm.filePath, d.startRow, d.startColumn, d.endRow, d.endColumn
          FROM ast AS n JOIN names nn ON nn.astNode = n.astId
            JOIN names AS dn ON nn.name = dn.name AND nn.namespace = dn.namespace
            JOIN ast AS d ON d.astId = dn.astNode
            JOIN modules nm ON n.module = nm.moduleId
            JOIN modules dm ON d.module = dm.moduleId
          WHERE nm.filePath = ? AND n.startRow <= ? AND n.endRow >= ? AND n.startColumn <= ? AND n.endColumn >= ?
      |] `mappend` (fromString $ maybe "" ((" AND dn.isDefined = " ++) . show) onlyDefinitions))
      (file, row, row, col, col)

getHoverInfo :: Connection -> FilePath -> Int -> Int -> IO [(Maybe String, Bool, String, Int, Int, Int, Int, Maybe String)]
getHoverInfo conn file row col =
  query
    conn
    [sql| 
      SELECT tn.type, nn.isDefined, nn.name, n.startRow, n.startColumn, n.endRow, n.endColumn, c.commentText
        FROM ast n
        JOIN names nn ON nn.astNode = n.astId
        JOIN modules nm ON n.module = nm.moduleId
        LEFT JOIN types tn ON nn.name = tn.typedName AND nn.namespace = tn.typeNamespace
        LEFT JOIN names dn ON nn.name = dn.name AND dn.isDefined = true
        LEFT JOIN ast da ON dn.namedDefinitionRow = da.startRow AND dn.namedDefinitionColumn = da.startColumn 
          AND dn.namedDefinitionEndRow = da.endRow AND dn.namedDefinitionEndColumn = da.endColumn
        LEFT JOIN definitions d ON d.astNode = da.astId
        LEFT JOIN comments c ON c.targetDefinition = d.definitionId
        WHERE nm.filePath = ? AND n.startRow <= ? AND n.endRow >= ? AND n.startColumn <= ? AND n.endColumn >= ?
    |]
    (file, row, row, col, col)

logErrorMessage :: Connection -> UTCTime -> String -> String -> IO ()
logErrorMessage conn time context message
  = void $ execute conn "INSERT INTO errorLogs (time, context, message) VALUES (?, ?, ?)" (time, context, message)

getErrors :: Connection -> IO [(String, String)]
getErrors conn = query_ conn "SELECT context, message FROM errorLogs"

listenToModuleClean :: Connection -> IO ()
listenToModuleClean conn = void $ execute_ conn "LISTEN module_clean"

cleanModuleFromDB :: Connection -> FilePath -> IO ()
cleanModuleFromDB conn filePath = do
  moduleIds <- query conn "SELECT moduleId FROM modules WHERE filePath = ?" [filePath]
  forM_ moduleIds $ \[moduleId] -> cleanRelatedData conn moduleId

cleanModulesFromDB :: Connection -> FilePath -> IO ()
cleanModulesFromDB conn filePath = do
  moduleIds <- query conn "SELECT moduleId FROM modules WHERE filePath LIKE '%' || ? || '%'" [filePath]
  forM_ moduleIds $ \[moduleId] -> cleanRelatedData conn moduleId

cleanRelatedData :: Connection -> Int -> IO ()
cleanRelatedData conn moduleId = void $ do
  execute conn "DELETE FROM definitions WHERE module = ?" [moduleId]
  execute conn "DELETE FROM comments WHERE module = ?" [moduleId]
  execute conn "DELETE FROM names WHERE module = ?" [moduleId]
  execute conn "DELETE FROM thRanges WHERE module = ?" [moduleId]
  execute conn "DELETE FROM ast WHERE module = ?" [moduleId]
  execute conn "DELETE FROM modules WHERE moduleId = ?" [moduleId]
  execute_ conn "DELETE FROM types WHERE NOT EXISTS (SELECT 1 FROM names WHERE typedName = name AND typeNamespace = namespace)"

reinitializeTablesIfNeeded :: Connection -> IO ()
reinitializeTablesIfNeeded conn = do
  res <- try $ query_ conn "SELECT versionNumber FROM version" :: IO (Either SomeException [[Int]])
  case res of
    Right [[r]] | databaseSchemaVersion == r -> return ()
    _ -> reinitializeTables conn

reinitializeTables :: Connection -> IO ()
reinitializeTables conn = do
  void $ execute_ conn "DROP OWNED BY SESSION_USER"
  initializeTables conn

initializeTables :: Connection -> IO ()
initializeTables conn = void $ execute conn databaseSchema (Only databaseSchemaVersion)

databaseSchemaVersion :: Int
databaseSchemaVersion = 4

databaseSchema :: Query
databaseSchema = [sql|

  CREATE TABLE version ( versionNumber INT NOT NULL );
  INSERT INTO version (versionNumber) VALUES (?);

  CREATE TABLE errorLogs
    ( time TIMESTAMP WITH TIME ZONE NOT NULL
    , context TEXT NOT NULL
    , message TEXT NOT NULL
    );

  CREATE TABLE modules
    ( moduleId SERIAL PRIMARY KEY
    , filePath TEXT UNIQUE NOT NULL
    , unitId TEXT NOT NULL
    , moduleName TEXT NOT NULL
    , compiledTime TIMESTAMP WITH TIME ZONE NOT NULL
    , loadingState INT NOT NULL
    , compiledSource TEXT NOT NULL
    , modifiedTime TIMESTAMP WITH TIME ZONE
    , modifiedFileDiffs TEXT
    );

  CREATE TABLE ast 
    ( module INT NOT NULL
    , CONSTRAINT fk_ast_module FOREIGN KEY(module) REFERENCES modules(moduleId)
    , astId SERIAL PRIMARY KEY
    , startRow INT NOT NULL
    , startColumn INT NOT NULL
    , endRow INT NOT NULL
    , endColumn INT NOT NULL
    );

  CREATE TABLE names 
    ( module INT NOT NULL
    , astNode INT NOT NULL
    , CONSTRAINT fk_name_ast FOREIGN KEY(astNode) REFERENCES ast(astId)
    , isDefined BOOL NOT NULL
    , namedDefinitionRow INT
    , namedDefinitionColumn INT
    , namedDefinitionEndRow INT
    , namedDefinitionEndColumn INT
    , name TEXT NOT NULL
    , namespace INT
    );

  CREATE TABLE definitions 
    ( definitionId SERIAL PRIMARY KEY
    , module INT
    , CONSTRAINT fk_def_module FOREIGN KEY(module) REFERENCES modules(moduleId)
    , astNode INT
    , CONSTRAINT fk_def_ast FOREIGN KEY(astNode) REFERENCES ast(astId)
    , parentDefinition INT
    , CONSTRAINT fk_def_parent FOREIGN KEY(parentDefinition) REFERENCES definitions(definitionId)
    , definitionKind INT NOT NULL
    );

  CREATE TABLE comments
    ( module INT NOT NULL
    , CONSTRAINT fk_comment_module FOREIGN KEY(module) REFERENCES modules(moduleId)
    , targetDefinition INT NOT NULL
    , CONSTRAINT fk_comment_def FOREIGN KEY(targetDefinition) REFERENCES definitions(definitionId)
    , commentText TEXT NOT NULL
    );

  CREATE TABLE types 
    ( typedName TEXT NOT NULL
    , typeNamespace INT
    , type TEXT NOT NULL
    );

  CREATE TABLE thRanges 
    ( module INT NOT NULL
    , astNode INT NOT NULL
    , CONSTRAINT fk_type_ast FOREIGN KEY(astNode) REFERENCES ast(astId)
    );

  CREATE OR REPLACE FUNCTION notifyModulesFunction()
    RETURNS trigger
    LANGUAGE plpgsql AS $$
      BEGIN
        IF NEW.modifiedFileDiffs IS NULL
          THEN PERFORM pg_notify('module_clean', NEW.filePath);
        END IF;
        RETURN NEW;
      END;
    $$;

  CREATE OR REPLACE TRIGGER notifyModulesTrigger
    AFTER INSERT OR UPDATE
    ON modules
    FOR EACH ROW
    EXECUTE FUNCTION notifyModulesFunction();

|]
