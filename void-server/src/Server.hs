{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      :  Server
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module Server where

import API.Fulfillment
import API.Token
import App
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Network.HTTP.Client (Manager)
import Servant
import Servant.Auth.Server

type API = TokenAPI :<|> (Auth '[JWT] Token :> FulfillmentAPI)

protected :: AuthResult Token -> ServerT FulfillmentAPI App
protected (Authenticated AccessToken{userID}) = fulfillmentAPI userID
protected _ = throwAll err401

runAppAsHandler :: Env -> App a -> Handler a
runAppAsHandler env app = runReaderT app env

server :: Env -> Server API
server env = hoistServerWithContext
  (Proxy :: Proxy API)
  (Proxy :: Proxy '[CookieSettings, JWTSettings])
  (runAppAsHandler env)
  (tokenAPI :<|> protected)

app :: Context '[CookieSettings, JWTSettings] -> Env -> Application
app cfg env = serveWithContext (Proxy :: Proxy API) cfg $ server env
