{-# LANGUAGE DeriveGeneric #-}

module InsertUser ( InsertUser(..) ) where

import Database.SQLite.Simple
import GHC.Generics
import Data.Aeson

data InsertUser = InsertUser
  { firstName :: String
  , lastName  :: String
  } deriving (Eq, Show, Generic)

instance ToJSON InsertUser where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON InsertUser

instance FromRow InsertUser where
  fromRow = InsertUser <$> field <*> field

instance ToRow InsertUser where
  toRow (InsertUser fname lname) = toRow (fname, lname)
