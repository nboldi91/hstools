{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Language.Haskell.HsTools.Database where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch as Catch
import Control.Monad.IO.Unlift
import Data.Maybe
import Data.Int (Int64)
import Data.Time.Clock
import Data.String
import Database.PostgreSQL.Simple hiding (query, query_, execute_, execute, executeMany, returning)
import qualified Database.PostgreSQL.Simple as PLSQL
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Language.Haskell.HsTools.SourcePosition (Range(..), startLine, startCol, endLine, endCol)

data LoadingState = NotLoaded | SourceSaved | NamesLoaded | TypesLoaded
    deriving (Show, Enum, Eq, Ord)

data Namespace = TyVarNS | TyConNS | DataConNS | ValNS | VarNS | ModuleNS deriving (Show, Enum, Eq, Ord)

data DefinitionKind
  = DefModule | DefSignature | DefInstance | DefPatternSynonym | DefClassOpSignature | DefValue | DefTypeClass
  | DefParameter | DefTypeDecl | DefConstructor | DefCtorArg | DefForeignExport | DefForeignImport
  deriving (Show, Eq, Ord, Enum)

data FullName = FullName
  { fnName :: String
  , fnLocalName :: Maybe String
  , fnNamespace :: Maybe Namespace
  } deriving (Show, Eq, Ord)

data FullRange a = FullRange
  { frModule :: Int
  , frRange :: Range a
  }

isSignatureDef :: DefinitionKind -> Bool
isSignatureDef DefSignature = True
isSignatureDef DefClassOpSignature = True
isSignatureDef _ = False

getCompiledTime :: DBMonad m => FilePath -> m (Maybe UTCTime)
getCompiledTime filePath = fmap (fmap head . listToMaybe) $ query
  [sql|
    SELECT compiledTime
    FROM modules
    WHERE filePath = ?
  |]
  (Only filePath)

getModuleIdLoadingState :: DBMonad m => FilePath -> m (Maybe (Int, LoadingState))
getModuleIdLoadingState filePath = do
  res <- query
    [sql| 
      SELECT moduleId, loadingState
      FROM modules
      WHERE filePath = ?
    |]
    (Only filePath)
  return $ case res of
    [(moduleId, loadingState)] -> Just (moduleId, toEnum loadingState)
    [] -> Nothing
    mods -> error $ "getModuleIdLoadingState: filePath should be unique, " ++ show filePath ++ ": " ++ show mods

updateLoadingState :: DBMonad m => Int -> LoadingState -> m ()
updateLoadingState moduleId newLoadingState =
  void $ execute
    [sql|
      UPDATE modules
      SET loadingState = ?
      WHERE moduleId = ?
    |]
    (fromEnum newLoadingState, moduleId)

updateFileDiffs :: DBMonad m => FilePath -> UTCTime -> Maybe String -> m ()
updateFileDiffs filePath modifiedTime serializedDiff =
  void $ execute
    [sql|
      UPDATE modules
      SET modifiedTime = ?, modifiedFileDiffs = ?
      WHERE filePath = ?
    |]
    (modifiedTime, serializedDiff, filePath)

getAllModifiedFileDiffs :: DBMonad m => m [(FilePath, Maybe String, Maybe UTCTime)]
getAllModifiedFileDiffs = query_
  [sql|
    SELECT filePath, modifiedFileDiffs, modifiedTime
    FROM modules
  |]

getCompiledSource :: DBMonad m => FilePath -> m (Maybe String)
getCompiledSource filePath
  = fmap (fmap head . listToMaybe) $ query
      [sql|
        SELECT compiledSource
        FROM modules
        WHERE filePath = ?
          AND compiledSource IS NOT NULL
      |]
      (Only filePath)

getModifiedTimeAndFileDiffs :: DBMonad m => FilePath -> m (Maybe String, Maybe UTCTime)
getModifiedTimeAndFileDiffs filePath =
  fromMaybe (Nothing, Nothing) . listToMaybe <$> query
    [sql|
      SELECT modifiedFileDiffs, modifiedTime
      FROM modules
      WHERE filePath = ?
    |]
    (Only filePath)

getModifiedFileDiffs :: DBMonad m => FilePath -> m (Maybe String)
getModifiedFileDiffs filePath
  = fmap (fmap head . listToMaybe) $ query
      [sql|
        SELECT modifiedFileDiffs
        FROM modules
        WHERE filePath = ?
          AND modifiedFileDiffs IS NOT NULL
      |]
      (Only filePath)

insertModule :: DBMonad m => FilePath -> UTCTime -> String -> String -> String -> m Int
insertModule fullPath roundedModificationTime moduleName unitId content
  = head . head <$> query 
      [sql|
        INSERT INTO modules (filePath, compiledTime, moduleName, unitId, loadingState, compiledSource, modifiedTime, modifiedFileDiffs)
        VALUES (?, ?, ?, ?, 0, ?, null, null)
        RETURNING moduleId
      |]
      (fullPath, roundedModificationTime, moduleName, unitId, content)

getAstNodes :: DBMonad m => Int -> m [(Int, Int, Int, Int, Int)]
getAstNodes moduleId =
  query
    [sql|
      SELECT startRow, startColumn, endRow, endColumn, astId
      FROM ast
      WHERE module = ?
    |]
    (Only moduleId)

getAllNames :: DBMonad m => m [(Int, Int, FullName, Maybe String, Bool)]
getAllNames = 
  map convertName <$> query_
    [sql|
      SELECT startRow, startColumn, name, localName, namespace, type, isDefined
      FROM ast AS n
      JOIN names nn
        ON nn.astNode = n.astId
      LEFT JOIN types tt
        ON tt.typedName = nn.name
        AND ((tt.typedLocalName IS NULL AND nn.localName IS NULL) OR tt.typedLocalName = nn.localName)
        AND tt.typedNamespace = nn.namespace
      ORDER BY startRow, startColumn
    |]
  where
    convertName (startRow, startColumn, name, localName, namespace, typ, isDefined) =
      (startRow, startColumn, FullName name localName (fmap toEnum namespace), typ, isDefined)

persistAst :: DBMonad m => [FullRange a] -> m [Int]
persistAst asts = fmap head <$> returning
  [sql|
    INSERT INTO ast (module, startRow, startColumn, endRow, endColumn)
    VALUES (?, ?, ?, ?, ?)
    RETURNING astId
  |]
  (map convert asts)
  where
    convert (FullRange mod rng) = (mod, startLine rng, startCol rng, endLine rng, endCol rng)

persistDefinitions :: DBMonad m => [(Int, Int, DefinitionKind)] -> m [Int]
persistDefinitions definitions = fmap head <$> returning
  [sql|
    INSERT INTO definitions (module, astNode, definitionKind)
    VALUES (?, ?, ?)
    RETURNING definitionId
  |]
  (map (\(a,b,c) -> (a,b, fromEnum c)) definitions)

persistName :: DBMonad m => [(Int, Int, FullName, Bool, Maybe (Range a))] -> m ()
persistName names = void $ executeMany
  [sql|
    INSERT INTO names (module, astNode, name, localName, namespace, isDefined, namedDefinitionRow, namedDefinitionColumn, namedDefinitionEndRow, namedDefinitionEndColumn)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  |]
  (map convertRecord names)
  where
    convertRecord (mod, ast, FullName name localName namespace, isDefined, rn) =
      (mod, ast, name, localName, fmap fromEnum namespace, isDefined, fmap startLine rn, fmap startCol rn, fmap endLine rn, fmap endCol rn)

persistTypes :: DBMonad m => [(FullName, String)] -> m ()
persistTypes types = void $ executeMany
  [sql|
    INSERT INTO types (typedName, typedLocalName, typedNamespace, type)
    VALUES (?, ?, ?, ?)
  |]
  (map convertType types)
  where
    convertType (FullName name localName namespace, typ) =
      (name, localName, fmap fromEnum namespace, typ)

persistTHRange' :: DBMonad m => Int -> Int -> m ()
persistTHRange' mod astNode = void $ execute
  [sql|
    INSERT INTO thRanges (module, astNode)
    VALUES (?, ?)
  |]
  (mod, astNode)

persistComments :: DBMonad m => [(Int, Int, String)] -> m ()
persistComments comments = void $ executeMany
  [sql|
    INSERT INTO comments (module, targetDefinition, commentText)
    VALUES (?, ?, ?)
  |]
  comments

getAllComments :: DBMonad m => m [(Int, Int, String)]
getAllComments = query_
  [sql|
    SELECT startRow, startColumn, commentText
    FROM ast a
    JOIN comments c
      ON c.targetDefinition = a.astId
    ORDER BY startRow, startColumn
  |]

persistMain :: DBMonad m => Int -> String -> m ()
persistMain mod nm = void $ execute
  [sql|
    INSERT INTO mains (module, name)
    VALUES (?, ?)
  |]
  (mod, nm)

getAllMains :: DBMonad m => m [[String]] -- not sure why this is list of strings
getAllMains = query_
  [sql|
    SELECT name
    FROM mains
  |]

getAllDefinitions :: DBMonad m => m [(DefinitionKind, Maybe String, Int, Int, Int, Int)]
getAllDefinitions = fmap (\(k,n,a,b,c,d) -> (toEnum k,n,a,b,c,d)) <$> query_ 
  [sql| 
    SELECT definitionKind, name, startRow, startColumn, endRow, endColumn
    FROM definitions d
    JOIN ast a
      ON d.astNode = a.astId
    LEFT JOIN names n
      ON a.startRow = n.namedDefinitionRow
      AND a.startColumn = n.namedDefinitionColumn
      AND a.endRow = n.namedDefinitionEndRow
      AND a.endColumn = n.namedDefinitionEndColumn
    ORDER BY startRow, startColumn
  |]

getTHRanges :: DBMonad m => Int -> m [(Int, Int, Int, Int)]
getTHRanges moduleId = query 
  [sql|
    SELECT startRow, startColumn, endRow, endColumn
    FROM thRanges t
    JOIN ast a
      ON t.astNode = a.astId
    WHERE a.module = ?
  |]
  (Only moduleId)

getAstId :: DBMonad m => FullRange a -> m Int
getAstId (FullRange moduleId rn) = head . head <$> query 
  [sql|
    SELECT astId
    FROM ast
    WHERE module = ?
      AND startRow = ?
      AND startColumn = ?
      AND endRow = ?
      AND endColumn = ?
  |]
  (moduleId, startLine rn, startCol rn, endLine rn, endCol rn)

insertAstId :: DBMonad m => FullRange a -> m Int
insertAstId (FullRange moduleId rn) = head . head <$> returning 
  [sql|
    INSERT INTO ast (module, startRow, startColumn, endRow, endColumn)
    VALUES (?, ?, ?, ?, ?)
    RETURNING astId
  |]
  [(moduleId, startLine rn, startCol rn, endLine rn, endCol rn)]

getMatchingNames :: DBMonad m => FilePath -> Int -> Int -> Maybe Bool -> m [(String, Int, Int, Int, Int)]
getMatchingNames file row col onlyDefinitions = query queryWithIsDefined (file, row, row, col, col)
  where
    queryWithoutIsDefined = 
      [sql|
          SELECT dm.filePath, d.startRow, d.startColumn, d.endRow, d.endColumn
            FROM ast AS n
              JOIN names nn
                ON nn.astNode = n.astId
              JOIN names AS dn
                ON nn.name = dn.name
                  AND ((nn.localName IS NULL AND dn.localName IS NULL) OR nn.localName = dn.localName)
                  AND nn.namespace = dn.namespace
              JOIN ast AS d
                ON d.astId = dn.astNode
              JOIN modules nm
                ON n.module = nm.moduleId
              JOIN modules dm
                ON d.module = dm.moduleId
            WHERE nm.filePath = ?
              AND n.startRow <= ?
              AND n.endRow >= ?
              AND n.startColumn <= ?
              AND n.endColumn >= ?
        |]
    queryWithIsDefined = queryWithoutIsDefined `mappend` (fromString $ maybe "" ((" AND dn.isDefined = " ++) . show) onlyDefinitions)

getHoverInfo :: DBMonad m => FilePath -> Int -> Int -> m [(Maybe String, Bool, String, Int, Int, Int, Int, Maybe String)]
getHoverInfo file row col =
  query
    [sql| 
      SELECT tn.type, nn.isDefined, nn.name, n.startRow, n.startColumn, n.endRow, n.endColumn, c.commentText
        FROM ast n
        JOIN names nn
          ON nn.astNode = n.astId
        JOIN modules nm
          ON n.module = nm.moduleId
        LEFT JOIN types tn
          ON nn.name = tn.typedName
            AND ((tn.typedLocalName IS NULL AND nn.localName IS NULL) OR nn.localName = tn.typedLocalName)
            AND nn.namespace = tn.typedNamespace
        LEFT JOIN names dn
          ON nn.name = dn.name
            AND dn.isDefined = true
        LEFT JOIN ast da
          ON dn.namedDefinitionRow = da.startRow
            AND dn.namedDefinitionColumn = da.startColumn 
            AND dn.namedDefinitionEndRow = da.endRow
            AND dn.namedDefinitionEndColumn = da.endColumn
        LEFT JOIN definitions d
          ON d.astNode = da.astId
        LEFT JOIN comments c
          ON c.targetDefinition = d.definitionId
        WHERE nm.filePath = ?
          AND n.startRow <= ?
          AND n.endRow >= ?
          AND n.startColumn <= ?
          AND n.endColumn >= ?
    |]
    (file, row, row, col, col)

listenToModuleClean :: DBMonad m => m ()
listenToModuleClean = void $ execute_ "LISTEN module_clean"

cleanModuleFromDB :: DBMonad m => FilePath -> m ()
cleanModuleFromDB filePath = do
  moduleIds <- query 
    [sql|
      SELECT moduleId
      FROM modules
      WHERE filePath = ?
    |]
    [filePath]
  forM_ moduleIds $ \[moduleId] -> cleanRelatedData moduleId

cleanModulesFromDB :: DBMonad m => FilePath -> m ()
cleanModulesFromDB filePath = do
  moduleIds <- query 
    [sql|
      SELECT moduleId
      FROM modules
      WHERE filePath LIKE '%' || ? || '%'
    |]
    [filePath]
  forM_ moduleIds $ \[moduleId] -> cleanRelatedData moduleId

cleanRelatedData :: DBMonad m => Int -> m ()
cleanRelatedData moduleId = void $ do
  mapM_
    (\table -> execute (fromString $ "DELETE FROM " ++ table ++ " WHERE module = ?") [moduleId])
    ["mains", "definitions", "comments", "names", "thRanges", "ast"]
  execute "DELETE FROM modules WHERE moduleId = ?" [moduleId]
  -- TODO: this could be foreign key?
  -- FIXME: condition not update for local names
  execute_ "DELETE FROM types WHERE NOT EXISTS (SELECT 1 FROM names WHERE typedName = name AND typedLocalName = localName AND typedNamespace = namespace)"

reinitializeTablesIfNeeded :: DBMonad m => m ()
reinitializeTablesIfNeeded = do
  res <- Catch.try @_ @SomeException $ query_ "SELECT versionNumber FROM version"
  case res of
    Right [[r]] | databaseSchemaVersion == r -> return ()
    _ -> reinitializeTables

reinitializeTables :: DBMonad m => m ()
reinitializeTables = do
  void $ execute_ "DROP OWNED BY SESSION_USER"
  void $ Catch.try @_ @SomeException $ execute_ "TRUNCATE version"
  initializeTables

initializeTables :: DBMonad m => m ()
initializeTables = void $ execute databaseSchema (Only databaseSchemaVersion)

databaseSchemaVersion :: Int
databaseSchemaVersion = 9

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

  CREATE INDEX ON modules(unitId, moduleName);

  CREATE TABLE ast 
    ( module INT NOT NULL
    , astId SERIAL PRIMARY KEY
    , startRow INT NOT NULL
    , startColumn INT NOT NULL
    , endRow INT NOT NULL
    , endColumn INT NOT NULL
    );

  CREATE INDEX ON ast USING HASH (module);
  CREATE INDEX ON ast USING BTREE (startRow, startColumn);
  CREATE INDEX ON ast USING BTREE (endRow, endColumn);

  CREATE TABLE names 
    ( module INT NOT NULL
    , astNode INT NOT NULL
    , isDefined BOOL NOT NULL
    , namedDefinitionRow INT
    , namedDefinitionColumn INT
    , namedDefinitionEndRow INT
    , namedDefinitionEndColumn INT
    , name TEXT NOT NULL
    , localName TEXT
    , namespace INT
    );

  CREATE INDEX ON names USING HASH (module);
  CREATE INDEX ON names USING HASH (astNode);
  CREATE INDEX ON names USING BTREE (namedDefinitionRow, namedDefinitionColumn);
  CREATE INDEX ON names USING BTREE (namedDefinitionEndRow, namedDefinitionEndColumn);
  CREATE INDEX ON names USING HASH (name);
  CREATE INDEX ON names USING HASH (localName);
  CREATE INDEX ON names USING HASH (namespace);

  CREATE TABLE definitions 
    ( definitionId SERIAL PRIMARY KEY
    , module INT
    , astNode INT
    , parentDefinition INT
    , definitionKind INT NOT NULL
    );
    
  CREATE INDEX ON definitions USING HASH (module);
  CREATE INDEX ON definitions USING HASH (astNode);
  CREATE INDEX ON definitions USING HASH (parentDefinition);

  CREATE TABLE comments
    ( module INT NOT NULL
    , targetDefinition INT NOT NULL
    , commentText TEXT NOT NULL
    );

  CREATE INDEX ON comments USING HASH (module);

  CREATE TABLE types 
    ( typedName TEXT NOT NULL
    , typedLocalName TEXT
    , typedNamespace INT
    , type TEXT NOT NULL
    );

  CREATE INDEX ON types USING HASH (typedName);
  CREATE INDEX ON types USING HASH (typedLocalName);
  CREATE INDEX ON types USING HASH (typedNamespace);

  CREATE TABLE thRanges 
    ( module INT NOT NULL
    , astNode INT NOT NULL
    );

  CREATE INDEX ON thRanges USING HASH (module);
  CREATE INDEX ON thRanges USING HASH (astNode);

  CREATE TABLE mains 
    ( module INT NOT NULL
    , name TEXT NOT NULL
    );

  CREATE INDEX ON mains USING HASH (module);
  CREATE INDEX ON mains USING HASH (name);

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

-- An additional layer on top of database operations to trace them

execute :: (DBMonad m, ToRow q, Show q) => Query -> q -> m Int64
execute query input = do
  conn <- getConnection
  wrapLogging ("execute: " ++ show query ++ " with inputs " ++ show input) $
    liftIO $ PLSQL.execute conn query input

execute_ :: DBMonad m => Query -> m Int64
execute_ query = do
  conn <- getConnection
  wrapLogging ("execute_: " ++ show query) $
    liftIO $ PLSQL.execute_ conn query

executeMany :: (DBMonad m, ToRow q, Show q) => Query -> [q] -> m Int64
executeMany query input = do
  conn <- getConnection
  logFullData <- shouldLogFullData
  let fullData = if logFullData then " with inputs " ++ show input else ""
  wrapLogging ("executeMany: " ++ show query ++ fullData) $
    liftIO $ PLSQL.executeMany conn query input  

query :: (DBMonad m, ToRow q, FromRow r, Show q) => Query -> q -> m [r]
query query input = do
  conn <- getConnection
  wrapLogging ("query: " ++ show query ++ " with inputs " ++ show input) $
    liftIO $ PLSQL.query conn query input

query_ :: (DBMonad m, FromRow r) => Query -> m [r]
query_ query = do
  conn <- getConnection
  wrapLogging ("query_: " ++ show query) $
    liftIO $ PLSQL.query_ conn query

returning :: (DBMonad m, ToRow q, FromRow r, Show q) => Query -> [q] -> m [r]
returning query input = do
  conn <- getConnection
  logFullData <- shouldLogFullData
  let fullData = if logFullData then " with inputs " ++ show input else ""
  wrapLogging ("returning: " ++ show query ++ fullData) $
    liftIO $ PLSQL.returning conn query input    

wrapLogging :: DBMonad m => String -> m a -> m a
wrapLogging query action = do
  logOperation $ "Executing " ++ query
  startTime <- liftIO getCurrentTime
  res <- action
  endTime <- liftIO getCurrentTime
  logPerformance $ "Query took " ++ show (diffUTCTime endTime startTime) ++ " seconds"
  return res

class (MonadIO m, MonadCatch m, MonadUnliftIO m) => DBMonad m where
  getConnection :: m Connection
  logOperation :: String -> m ()
  logPerformance :: String -> m ()
  shouldLogFullData :: m Bool
