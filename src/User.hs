{-# LANGUAGE DeriveGeneric #-}

module User ( User(..) ) where

import Database.SQLite.Simple
import GHC.Generics
import Data.Aeson

data User = User
  { id        :: Int
  , firstName :: String
  , lastName  :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON User

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToRow User where
  toRow (User id_ fname lname) = toRow (id_, fname, lname)
