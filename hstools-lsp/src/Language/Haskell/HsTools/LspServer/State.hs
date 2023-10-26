{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.HsTools.LspServer.State where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)

import Language.Haskell.HsTools.LspServer.FileRecords
import Language.Haskell.HsTools.Utils (Verbosity(..))

loadConfig :: Config -> A.Value -> Either T.Text Config
loadConfig config (A.Object (A.lookup "hstools" -> (Just (A.Object assoc))))
  = Right $ config 
  { cfPostgresqlConnectionString = fromMaybe (cfPostgresqlConnectionString config) $
      T.unpack <$> (fromString =<< psqlstr)
  , cfVerbosity = fromMaybe (cfVerbosity config) $
      readMaybe . ("Verbosity" ++) . T.unpack =<< fromString =<< verbosity
  , cfLogFilePath = fromMaybe (cfLogFilePath config) $
      T.unpack <$> (fromString =<< logFilePath)
  }
  where
    psqlstr = A.lookup "postgresqlConnectionString" assoc
    verbosity = A.lookup "verbosity" assoc
    logFilePath = A.lookup "logFilePath" assoc
    fromString (A.String t) = Just t
    fromString _ = Nothing
loadConfig _ v = Left $ T.pack $ "Cannot parse options: " ++ show v

hsToolsDefaultConfig :: FileRecords -> Config
hsToolsDefaultConfig fr = Config
  { cfPostgresqlConnectionString = ""
  , cfConnection = Nothing
  , cfOperation = Nothing
  , cfFileRecords = fr
  , cfVerbosity = VerbositySilent
  , cfLogFilePath = ".hstools.log"
  }

data Config = Config
  { cfPostgresqlConnectionString :: String
  , cfConnection :: Maybe Connection
  , cfOperation :: Maybe String
  , cfFileRecords :: FileRecords
  , cfVerbosity :: Verbosity
  , cfLogFilePath :: FilePath
  }
