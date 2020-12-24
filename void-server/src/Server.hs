-- Copyright (C) 2020  Jared Beller
-- This file is part of void-server
--
-- void-server is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <https://www.gnu.org/licenses/>.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

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
