{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RankNTypes #-}

module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.HTTP.Types.Status (ok200)
import Network.Wai.Handler.Warp
import Servant
import Control.Monad.IO.Class
import Control.Monad.Reader
import Database.SQLite.Simple
import Control.Monad.Except
import Data.Text.Lazy.Encoding (encodeUtf8)
import User
import InsertUser

import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LB
import qualified Servant.Docs as Docs
import qualified SQLite as SQL

type App = ReaderT Connection (ExceptT ServerError IO)

-- |Types Implementing This Class Provide Access Registering And Retrieving Users Based On Authentication
class (MonadIO m) => UserRepo m where
    -- |Return A List Of All Users
    getUsers :: Maybe String -> m [User]
    -- |Return The User With The Matching ID
    getUserById :: Int -> Maybe String -> m User
    -- |Insert The User Into The Database
    insertUser :: Maybe String -> InsertUser -> m NoContent

instance UserRepo App where
    getUsers = SQL.getUsers
    getUserById = SQL.getUserById
    insertUser = SQL.insertUser

instance Docs.ToCapture (Capture "uid" Int) where
  toCapture _ = Docs.DocCapture "uid" "(integer) The ID of the person you want to fetch"

instance Docs.ToParam (QueryParam "apikey" String) where
    toParam _ = Docs.DocQueryParam "apikey" ["jahs7_3d", "hf5s_8hs", "dw4k83s_s7"] "API key to authenticate access" Docs.Normal

instance Docs.ToSample User where
  toSamples _ = Docs.samples [User 1 "Joshua" "Billson", User 2 "Romeo" "Billson"]
  
instance Docs.ToSample InsertUser where
  toSamples _ = Docs.samples [InsertUser "Joshua" "Billson", InsertUser "Romeo" "Billson"]

type API = "users" :> QueryParam "apikey" String :> Get '[JSON] [User]
      :<|> "users" :> Capture "uid" Int :> QueryParam "apikey" String :> Get '[JSON] User
      :<|> "users" :> QueryParam "apikey" String :> ReqBody '[JSON] InsertUser :> PostNoContent
      :<|> Raw

server :: (UserRepo m) => ServerT API m
server = getUsers :<|> getUserById  :<|> insertUser :<|> Tagged serveDocs
    where serveDocs _ response = response $ responseLBS ok200 [] docs

api :: Proxy API
api = Proxy

docs :: LB.ByteString
docs = encodeUtf8 . LT.pack . Docs.markdown . Docs.docs $ api

runner :: (forall x. App x -> Handler x)
runner application = liftIO (withConnection "data.db" (runExceptT . runReaderT application)) >>= liftEither

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api (hoistServer api runner server)