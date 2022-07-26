{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Errors
import Control.Monad
import Control.Monad.State
import Control.Exception
import Text.Read (readMaybe)
import qualified Data.ByteString.Char8 as BS

import Language.Haskell.HsTools.Utils.ShowTable

dropTables :: Connection -> IO Bool
dropTables conn = do
    execute_ conn "DROP TABLE IF EXISTS names;"
    execute_ conn "DROP TABLE IF EXISTS modules;"
    putStrLn "Dropped tables"
    return True

main = do
    putStrLn "Welcome to HsTools"
    putStrLn "Type 'Help' and hit Enter if you need help"
    runStateT applicationLoop initialState

    -- initTables conn

applicationLoop :: AppMonad ()
applicationLoop = do
    line <- liftIO getLine
    shouldContinue <- case readMaybe line of
        Just command -> executeCommand command
        Nothing -> do
            liftIO $ putStrLn $ "I did not understand the command " ++ line
            return True
    when shouldContinue applicationLoop

type AppMonad = StateT AppState IO

executeCommand :: Command -> AppMonad Bool
executeCommand Help = do
    liftIO $ putStrLn "Possible commands are \n\
        \Help\n\
        \Connect \"plsql connection string\"\n\
        \FindDefinition \"file path\" rowNumber columnNumber\n\
        \CleanDatabase\n\
        \Quit\n\
        \"
    return True
executeCommand (Connect connectionStr) = do
    conn <- liftIO $ connectPostgreSQL (BS.pack connectionStr)
    modify $ \s -> s { connection = Just conn } 
    return True
executeCommand (DebugQuery dq) = ensureConnection $ \conn -> do
    tryResult <- liftIO $ try $ do
        results <- query_ conn dq
        case results of
            r1:_ -> putStrLn $ showMat "  " $ map fieldName r1 : map (map fieldValue) (results :: [[GenericField]])
            [] -> putStrLn "No results"
    case tryResult of
        Right () -> return ()
        Left err -> liftIO $ putStrLn $ "Error during execution: " ++ show (err :: SqlError)
    return True
executeCommand (FindDefinition file row col) = ensureConnection $ \conn -> do
    names <- liftIO $ query
        conn
        "SELECT d.file, d.startRow, d.startColumn \
            \FROM names AS n JOIN names AS d ON n.name = d.name \
            \WHERE n.file = ? AND n.startRow <= ? AND n.endRow >= ? AND n.startColumn <= ? AND n.endColumn >= ? \
            \AND d.isDefined = TRUE"
        ( file, row, row, col, col )
    liftIO $ case names of
        [] -> putStrLn "Definition not found"
        -- TODO: toggle between definitions if there are multiple
        (file, startRow :: Int, startColumn :: Int):_ -> 
            putStrLn (file ++ " " ++ show startRow ++ ":" ++ show startColumn)
    return True
executeCommand CleanDatabase = ensureConnection (liftIO . dropTables) >> return True
executeCommand Quit = return False
    
ensureConnection = ensureConnectionDefault True

ensureConnectionDefault :: a -> (Connection -> AppMonad a) -> AppMonad a
ensureConnectionDefault def action = do
    conn <- gets connection
    case conn of Just activeConnection -> action activeConnection
                 Nothing -> do
                    liftIO $ putStrLn "This command needs an active connection. Run 'Connect' first."
                    return def

data AppState = AppState {
    connection :: Maybe Connection
}

data GenericField = GenericField { fieldName :: String, fieldValue :: String }

instance FromField GenericField where
    fromField f bs = return $ GenericField (maybe "" BS.unpack (name f)) (maybe "" BS.unpack bs)

initialState :: AppState
initialState = AppState Nothing

data Command = Help
             | Connect String
             | FindDefinition String Int Int
             | DebugQuery Query
             | CleanDatabase
             | Quit
    deriving (Show, Read)
