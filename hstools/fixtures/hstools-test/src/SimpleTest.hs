{-# LANGUAGE TemplateHaskell #-}
module SimpleTest where

data ColumnType = ColumnType
  { columnName :: String
  , columnValue :: Int
  } deriving (Show, Read)
