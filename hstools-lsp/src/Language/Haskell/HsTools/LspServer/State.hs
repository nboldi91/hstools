{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.HsTools.LspServer.State where

import Control.Concurrent.MVar
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)

import Language.Haskell.HsTools.LspServer.FileRecords
import Language.Haskell.HsTools.Utils (LogOptions(..), defaultLogOptions)

loadConfig :: Config -> A.Value -> Either T.Text Config
loadConfig config (A.Object (A.lookup "hstools" -> (Just (A.Object assoc))))
  = Right $ config 
  { cfPostgresqlConnectionString = fromMaybe (cfPostgresqlConnectionString config) $
      T.unpack <$> (fromString =<< A.lookup "postgresqlConnectionString" assoc)
  , cfLogOptions =
    LogOptions
      (fromMaybe (logOptionsHighLevel $ cfLogOptions config) $ fromBool =<< lookupLogOption "highLevel")
      (fromMaybe (logOptionsQueries $ cfLogOptions config) $ fromBool =<< lookupLogOption "queries")
      (fromMaybe (logOptionsPerformance $ cfLogOptions config) $ fromBool =<< lookupLogOption "performance")
      (fromMaybe (logOptionsFullData $ cfLogOptions config) $ fromBool =<< lookupLogOption "fullData")
      (fromMaybe (logOptionsOutputFile $ cfLogOptions config) $ fmap (emptyToNothing . T.unpack) (fromString =<< lookupLogOption "logFilePath"))
  }
  where
    lookupLogOption k =
      case A.lookup "logOptions" assoc of
        Just (A.Object km) -> A.lookup k km
        _ -> Nothing
    fromString (A.String t) = Just t
    fromString _ = Nothing
    emptyToNothing "" = Nothing
    emptyToNothing x = Just x
    fromBool (A.Bool t) = Just t
    fromBool _ = Nothing
loadConfig _ v = Left $ T.pack $ "Cannot parse options: " ++ show v

hsToolsDefaultConfig :: IO Config
hsToolsDefaultConfig = do
  fileRecords <- newEmptyMVar
  connection <- newEmptyMVar
  return $ Config
    { cfPostgresqlConnectionString = ""
    , cfConnection = connection
    , cfOperation = Nothing
    , cfFileRecords = fileRecords
    , cfLogOptions = defaultLogOptions
    }

data Config = Config
  { cfPostgresqlConnectionString :: String
  , cfConnection :: MVar Connection
  , cfOperation :: Maybe String
  , cfFileRecords :: FileRecords
  , cfLogOptions :: LogOptions
  }
