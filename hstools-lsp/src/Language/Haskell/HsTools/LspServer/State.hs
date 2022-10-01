{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Haskell.HsTools.LspServer.State where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)

import Language.Haskell.HsTools.LspServer.FileRecords

loadConfig :: Config -> A.Value -> Either T.Text Config
loadConfig config (A.Object (A.lookup "hstools" -> (Just (A.Object assoc))))
  = Right $ config 
  { cfPostgresqlConnectionString = fromMaybe (cfPostgresqlConnectionString config) $
      T.unpack <$> (fromString =<< psqlstr)
  }
  where
    psqlstr = A.lookup "postgresqlConnectionString" assoc
    fromString (A.String t) = Just t
    fromString _ = Nothing
loadConfig _ v = Left $ T.pack $ "Cannot parse options: " ++ show v

hsToolsDefaultConfig :: FileRecords -> Config
hsToolsDefaultConfig fr = Config
  { cfPostgresqlConnectionString = ""
  , cfConnection = Nothing
  , cfOperation = Nothing
  , cfFileRecords = fr
  }

data Config = Config
  { cfPostgresqlConnectionString :: String
  , cfConnection :: Maybe Connection
  , cfOperation :: Maybe String
  , cfFileRecords :: FileRecords
  }
