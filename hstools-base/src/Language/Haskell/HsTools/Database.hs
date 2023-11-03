{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Language.Haskell.HsTools.Database where

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Int (Int64)
import Data.Time.Clock
import Data.String
import Database.PostgreSQL.Simple hiding (query, query_, execute_, execute, executeMany, returning)
import qualified Database.PostgreSQL.Simple as PLSQL
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Language.Haskell.HsTools.Utils (DbConn(..), LogOptions(..), createLogger)
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

getCompiledTime :: DbConn -> FilePath -> IO (Maybe UTCTime)
getCompiledTime conn filePath = fmap (fmap head . listToMaybe) $ query conn
  [sql|
    SELECT compiledTime
    FROM modules
    WHERE filePath = ?
  |]
  (Only filePath)

getModuleIdLoadingState :: DbConn -> FilePath -> IO (Maybe (Int, LoadingState))
getModuleIdLoadingState conn filePath = do
  res <- query conn
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

updateLoadingState :: DbConn -> Int -> LoadingState -> IO ()
updateLoadingState conn moduleId newLoadingState =
  void $ execute conn
    [sql|
      UPDATE modules
      SET loadingState = ?
      WHERE moduleId = ?
    |]
    (fromEnum newLoadingState, moduleId)

updateFileDiffs :: DbConn -> FilePath -> UTCTime -> Maybe String -> IO ()
updateFileDiffs conn filePath modifiedTime serializedDiff =
  void $ execute conn
    [sql|
      UPDATE modules
      SET modifiedTime = ?, modifiedFileDiffs = ?
      WHERE filePath = ?
    |]
    (modifiedTime, serializedDiff, filePath)

getAllModifiedFileDiffs :: DbConn -> IO [(FilePath, Maybe String, Maybe UTCTime)]
getAllModifiedFileDiffs conn = query_ conn
  [sql|
    SELECT filePath, modifiedFileDiffs, modifiedTime
    FROM modules
  |]

getCompiledSource :: DbConn -> FilePath -> IO (Maybe String)
getCompiledSource conn filePath
  = fmap (fmap head . listToMaybe) $ query conn
      [sql|
        SELECT compiledSource
        FROM modules
        WHERE filePath = ?
          AND compiledSource IS NOT NULL
      |]
      (Only filePath)

getModifiedTimeAndFileDiffs :: DbConn -> FilePath -> IO (Maybe String, Maybe UTCTime)
getModifiedTimeAndFileDiffs conn filePath =
  fromMaybe (Nothing, Nothing) . listToMaybe <$> query conn
    [sql|
      SELECT modifiedFileDiffs, modifiedTime
      FROM modules
      WHERE filePath = ?
    |]
    (Only filePath)

getModifiedFileDiffs :: DbConn -> FilePath -> IO (Maybe String)
getModifiedFileDiffs conn filePath
  = fmap (fmap head . listToMaybe) $ query conn
      [sql|
        SELECT modifiedFileDiffs
        FROM modules
        WHERE filePath = ?
          AND modifiedFileDiffs IS NOT NULL
      |]
      (Only filePath)

insertModule :: DbConn -> FilePath -> UTCTime -> String -> String -> String -> IO Int
insertModule conn fullPath roundedModificationTime moduleName unitId content
  = head . head <$> query conn 
      [sql|
        INSERT INTO modules (filePath, compiledTime, moduleName, unitId, loadingState, compiledSource, modifiedTime, modifiedFileDiffs)
        VALUES (?, ?, ?, ?, 0, ?, null, null)
        RETURNING moduleId
      |]
      (fullPath, roundedModificationTime, moduleName, unitId, content)

getAstNodes :: DbConn -> Int -> IO [(Int, Int, Int, Int, Int)]
getAstNodes conn moduleId =
  query conn
    [sql|
      SELECT startRow, startColumn, endRow, endColumn, astId
      FROM ast
      WHERE module = ?
    |]
    (Only moduleId)

getAllNames :: DbConn -> IO [(Int, Int, FullName, Maybe String, Bool)]
getAllNames conn = 
  map convertName <$> query_ conn
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

persistAst :: DbConn -> [FullRange a] -> IO [Int]
persistAst conn asts = fmap head <$> returning conn
  [sql|
    INSERT INTO ast (module, startRow, startColumn, endRow, endColumn)
    VALUES (?, ?, ?, ?, ?)
    RETURNING astId
  |]
  (map convert asts)
  where
    convert (FullRange mod rng) = (mod, startLine rng, startCol rng, endLine rng, endCol rng)

persistDefinitions :: DbConn -> [(Int, Int, DefinitionKind)] -> IO [Int]
persistDefinitions conn definitions = fmap head <$> returning conn
  [sql|
    INSERT INTO definitions (module, astNode, definitionKind)
    VALUES (?, ?, ?)
    RETURNING definitionId
  |]
  (map (\(a,b,c) -> (a,b, fromEnum c)) definitions)

persistName :: DbConn -> [(Int, Int, FullName, Bool, Maybe (Range a))] -> IO ()
persistName conn names = void $ executeMany conn
  [sql|
    INSERT INTO names (module, astNode, name, localName, namespace, isDefined, namedDefinitionRow, namedDefinitionColumn, namedDefinitionEndRow, namedDefinitionEndColumn)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  |]
  (map convertRecord names)
  where
    convertRecord (mod, ast, FullName name localName namespace, isDefined, rn) =
      (mod, ast, name, localName, fmap fromEnum namespace, isDefined, fmap startLine rn, fmap startCol rn, fmap endLine rn, fmap endCol rn)

persistTypes :: DbConn -> [(FullName, String)] -> IO ()
persistTypes conn types = void $ executeMany conn
  [sql|
    INSERT INTO types (typedName, typedLocalName, typedNamespace, type)
    VALUES (?, ?, ?, ?)
  |]
  (map convertType types)
  where
    convertType (FullName name localName namespace, typ) =
      (name, localName, fmap fromEnum namespace, typ)

persistTHRange' :: DbConn -> Int -> Int -> IO ()
persistTHRange' conn mod astNode = void $ execute conn
  [sql|
    INSERT INTO thRanges (module, astNode)
    VALUES (?, ?)
  |]
  (mod, astNode)

persistComments :: DbConn -> [(Int, Int, String)] -> IO ()
persistComments conn comments = void $ executeMany conn
  [sql|
    INSERT INTO comments (module, targetDefinition, commentText)
    VALUES (?, ?, ?)
  |]
  comments

getAllComments :: DbConn -> IO [(Int, Int, String)]
getAllComments conn = query_ conn
  [sql|
    SELECT startRow, startColumn, commentText
    FROM ast a
    JOIN comments c
      ON c.targetDefinition = a.astId
    ORDER BY startRow, startColumn
  |]

persistMain :: DbConn -> Int -> String -> IO ()
persistMain conn mod nm = void $ execute conn
  [sql|
    INSERT INTO mains (module, name)
    VALUES (?, ?)
  |]
  (mod, nm)

getAllMains :: DbConn -> IO [[String]] -- not sure why this is list of strings
getAllMains conn = query_ conn
  [sql|
    SELECT name
    FROM mains
  |]

getAllDefinitions :: DbConn -> IO [(DefinitionKind, Maybe String, Int, Int, Int, Int)]
getAllDefinitions conn = fmap (\(k,n,a,b,c,d) -> (toEnum k,n,a,b,c,d)) <$> query_ conn 
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

getTHRanges :: DbConn -> Int -> IO [(Int, Int, Int, Int)]
getTHRanges conn moduleId = query conn 
  [sql|
    SELECT startRow, startColumn, endRow, endColumn
    FROM thRanges t
    JOIN ast a
      ON t.astNode = a.astId
    WHERE a.module = ?
  |]
  (Only moduleId)

getAstId :: DbConn -> FullRange a -> IO Int
getAstId conn (FullRange moduleId rn) = head . head <$> query conn 
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

insertAstId :: DbConn -> FullRange a -> IO Int
insertAstId conn (FullRange moduleId rn) = head . head <$> returning conn 
  [sql|
    INSERT INTO ast (module, startRow, startColumn, endRow, endColumn)
    VALUES (?, ?, ?, ?, ?)
    RETURNING astId
  |]
  [(moduleId, startLine rn, startCol rn, endLine rn, endCol rn)]

getMatchingNames :: DbConn -> FilePath -> Int -> Int -> Maybe Bool -> IO [(String, Int, Int, Int, Int)]
getMatchingNames conn file row col onlyDefinitions = query conn queryWithIsDefined (file, row, row, col, col)
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

getHoverInfo :: DbConn -> FilePath -> Int -> Int -> IO [(Maybe String, Bool, String, Int, Int, Int, Int, Maybe String)]
getHoverInfo conn file row col =
  query
    conn
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

logErrorMessage :: Connection -> UTCTime -> String -> String -> IO ()
logErrorMessage conn time context message = void $ PLSQL.execute conn
  [sql|
    INSERT INTO errorLogs (time, context, message)
    VALUES (?, ?, ?)
  |]
  (time, context, message)

getErrors :: DbConn -> IO [(String, String)]
getErrors conn = query_ conn
  [sql|
    SELECT context, message
    FROM errorLogs
  |]

listenToModuleClean :: DbConn -> IO ()
listenToModuleClean conn = void $ execute_ conn "LISTEN module_clean"

cleanModuleFromDB :: DbConn -> FilePath -> IO ()
cleanModuleFromDB conn filePath = do
  moduleIds <- query conn 
    [sql|
      SELECT moduleId
      FROM modules
      WHERE filePath = ?
    |]
    [filePath]
  forM_ moduleIds $ \[moduleId] -> cleanRelatedData conn moduleId

cleanModulesFromDB :: DbConn -> FilePath -> IO ()
cleanModulesFromDB conn filePath = do
  moduleIds <- query conn 
    [sql|
      SELECT moduleId
      FROM modules
      WHERE filePath LIKE '%' || ? || '%'
    |]
    [filePath]
  forM_ moduleIds $ \[moduleId] -> cleanRelatedData conn moduleId

cleanRelatedData :: DbConn -> Int -> IO ()
cleanRelatedData conn moduleId = void $ do
  mapM_
    (\table -> execute conn (fromString $ "DELETE FROM " ++ table ++ " WHERE module = ?") [moduleId])
    ["mains", "definitions", "comments", "names", "thRanges", "ast"]
  execute conn "DELETE FROM modules WHERE moduleId = ?" [moduleId]
  -- TODO: this could be foreign key?
  -- FIXME: condition not update for local names
  execute_ conn "DELETE FROM types WHERE NOT EXISTS (SELECT 1 FROM names WHERE typedName = name AND typedLocalName = localName AND typedNamespace = namespace)"

reinitializeTablesIfNeeded :: DbConn -> IO ()
reinitializeTablesIfNeeded conn = do
  res <- try $ query_ conn "SELECT versionNumber FROM version" :: IO (Either SomeException [[Int]])
  case res of
    Right [[r]] | databaseSchemaVersion == r -> return ()
    _ -> reinitializeTables conn

reinitializeTables :: DbConn -> IO ()
reinitializeTables conn = do
  void $ execute_ conn "DROP OWNED BY SESSION_USER"
  void $ try @SomeException $ execute_ conn "TRUNCATE version"
  initializeTables conn

initializeTables :: DbConn -> IO ()
initializeTables conn = void $ execute conn databaseSchema (Only databaseSchemaVersion)

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

execute :: (ToRow q, Show q) => DbConn -> Query -> q -> IO Int64
execute conn query input =
  wrapLogging conn ("execute: " ++ show query ++ " with inputs " ++ show input) $
    PLSQL.execute (dbConnConnection conn) query input

execute_ :: DbConn -> Query -> IO Int64
execute_ conn query =
  wrapLogging conn ("execute_: " ++ show query) $
    PLSQL.execute_ (dbConnConnection conn) query

executeMany :: (ToRow q, Show q) => DbConn -> Query -> [q] -> IO Int64
executeMany conn query input =
  wrapLogging conn ("executeMany: " ++ show query ++ fullData) $
    PLSQL.executeMany (dbConnConnection conn) query input
  where
    fullData = if logOptionsFullData (dbConnLogOptions conn) then " with inputs " ++ show input else ""

query :: (ToRow q, FromRow r, Show q) => DbConn -> Query -> q -> IO [r]
query conn query input =
  wrapLogging conn ("query: " ++ show query ++ " with inputs " ++ show input) $
    PLSQL.query (dbConnConnection conn) query input

query_ :: FromRow r => DbConn -> Query -> IO [r]
query_ conn query =
  wrapLogging conn ("query_: " ++ show query) $
    PLSQL.query_ (dbConnConnection conn) query

returning :: (ToRow q, FromRow r, Show q) => DbConn -> Query -> [q] -> IO [r]
returning conn query input =
  wrapLogging conn ("returning: " ++ show query ++ fullData) $
    PLSQL.returning (dbConnConnection conn) query input
  where
    fullData = if logOptionsFullData (dbConnLogOptions conn) then " with inputs " ++ show input else ""

wrapLogging :: DbConn -> String -> IO a -> IO a
wrapLogging conn query action = do
    when (logOptionsQueries logOptions || logOptionsPerformance logOptions) $
      logger $ "Executing " ++ query
    startTime <- getCurrentTime
    res <- action
    endTime <- getCurrentTime
    when (logOptionsPerformance logOptions) $
      logger $ "Query took " ++ show (diffUTCTime endTime startTime) ++ " seconds"
    return res
  where
    logger = createLogger logOptions
    logOptions = dbConnLogOptions conn
