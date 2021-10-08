module UserRepo ( User(..)
                , getUsers
                , getUserById
                , insertUser
                , getNames
                ) where

import Control.Monad.Reader
import Database.SQLite.Simple
import Servant
import Control.Monad.Except
import Data.Aeson.TH (deriveJSON, defaultOptions)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field

instance ToRow User where
  toRow (User id_ fname lname) = toRow (id_, fname, lname)

getNames :: (MonadReader Connection m, MonadIO m, MonadError ServerError m) => Maybe String -> m [String]
getNames apiKey = do
    authenticate apiKey
    connection <- ask
    results <- liftIO $ query_ connection "SELECT (u.fname || ' ' || u.lname) FROM Users u;"
    return $ (\(Only str) -> str) <$> results

getUsers :: (MonadReader Connection m, MonadIO m, MonadError ServerError m) => Maybe String -> m [User]
getUsers apiKey = do
    authenticate apiKey
    connection <- ask
    liftIO $ query_ connection "SELECT * FROM Users;"


getUserById :: (MonadReader Connection m, MonadIO m, MonadError ServerError m) => Int -> Maybe String -> m User
getUserById uid apiKey = do
    authenticate apiKey
    connection <- ask
    userOrError <- liftIO $ userExists <$> query connection "SELECT * FROM Users WHERE uid=?" (Only uid)
    liftEither userOrError


insertUser :: (MonadReader Connection m, MonadIO m, MonadError ServerError m) => Maybe String -> User -> m NoContent
insertUser apiKey (User uid fname lname) = do
    authenticate apiKey
    connection <- ask
    liftIO $ execute connection "INSERT INTO Users (uid, fname, lname) VALUES (?, ?, ?);" (uid, fname, lname)
    return NoContent


authenticate :: (MonadError ServerError m) => Maybe String -> m ()
authenticate Nothing = throwError err401
authenticate (Just apiKey') =
    if apiKey' == apiKey then return () else throwError err401
    where apiKey = "hK0iP5dL7bW3fP3y"


userExists :: [User] -> Either ServerError User
userExists [] = Left err404
userExists (x:_) = Right x