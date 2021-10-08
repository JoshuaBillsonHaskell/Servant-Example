module UserRepo ( getUsers
                , getUserById
                , insertUser
                ) where

import Control.Monad.Reader
import Database.SQLite.Simple
import Servant
import Control.Monad.Except
import User
import InsertUser
import Control.Exception (catch)


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


insertUser :: (MonadReader Connection m, MonadIO m, MonadError ServerError m) => Maybe String -> InsertUser -> m NoContent
insertUser apiKey (InsertUser fname lname) = do
    authenticate apiKey
    connection <- ask
    result <- liftIO $ safeExecute connection "INSERT INTO Users (fname, lname) VALUES (?, ?);" (fname, lname)
    case result of
        Left err -> throwError err
        Right _ -> return NoContent


-- |Authenticates That The Provided API Key Is Correct And Throws A 401 Error If It Is Not
authenticate :: (MonadError ServerError m) => Maybe String -> m ()
authenticate Nothing = throwError err401
authenticate (Just apiKey') =
    if apiKey' == apiKey then return () else throwError err401
    where apiKey = "hK0iP5dL7bW3fP3y"


-- |Helper Function For Catching Exceptions Thrown When Inserting To The Database
catchInsert :: SQLError -> IO (Either ServerError ())
catchInsert _ = return $ Left err409


-- |Turns Exceptions Thrown While Executing A SQL Query Into Server Errors Which Can Be Handled Deterministically
safeExecute :: ToRow q => Connection -> Query -> q -> IO (Either ServerError ())
safeExecute conn query' q = do
    catch (tryExec conn query' q) catchInsert
    where tryExec conn' query'' q' = do
              execute conn' query'' q'
              return $ Right ()


-- |Either Returns A User From A List Of Users Or Returns A ServerError If The List Is Empty
userExists :: [User] -> Either ServerError User
userExists [] = Left err404
userExists (x:_) = Right x