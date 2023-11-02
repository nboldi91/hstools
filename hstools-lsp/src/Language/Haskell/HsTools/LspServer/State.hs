{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.HsTools.LspServer.State where

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
      T.unpack <$> (fromString =<< lookup "postgresqlConnectionString")
  , cfLogOptions =
    LogOptions
      (fromMaybe (logOptionsHighLevel $ cfLogOptions config) $ fromBool =<< lookup "logOptions.highLevel")
      (fromMaybe (logOptionsQueries $ cfLogOptions config) $ fromBool =<< lookup "logOptions.queries")
      (fromMaybe (logOptionsPerformance $ cfLogOptions config) $ fromBool =<< lookup "logOptions.performance")
      (fromMaybe (logOptionsFullData $ cfLogOptions config) $ fromBool =<< lookup "logOptions.fullData")
      (fromMaybe (logOptionsOutputFile $ cfLogOptions config) $ fmap (emptyToNothing . T.unpack) (fromString =<< lookup "logOptions.logFilePath"))
  }
  where
    lookup q = A.lookup q assoc
    fromString (A.String t) = Just t
    fromString _ = Nothing
    emptyToNothing "" = Nothing
    emptyToNothing x = Just x
    fromBool (A.Bool t) = Just t
    fromBool _ = Nothing
loadConfig _ v = Left $ T.pack $ "Cannot parse options: " ++ show v

hsToolsDefaultConfig :: FileRecords -> Config
hsToolsDefaultConfig fr = Config
  { cfPostgresqlConnectionString = ""
  , cfConnection = Nothing
  , cfOperation = Nothing
  , cfFileRecords = fr
  , cfLogOptions = defaultLogOptions
  }

data Config = Config
  { cfPostgresqlConnectionString :: String
  , cfConnection :: Maybe Connection
  , cfOperation :: Maybe String
  , cfFileRecords :: FileRecords
  , cfLogOptions :: LogOptions
  }
