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

{-# LANGUAGE TypeOperators #-}

module Server where

import API.Fulfillment
import API.Token
import App
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Network.HTTP.Client (Manager)
import Servant
import Servant.Auth.Server

type API = TokenAPI :<|> FulfillmentAPI

runAppAsHandler :: Env -> App a -> Handler a
runAppAsHandler env app = runReaderT app env

server :: Proxy API -> Env -> Server API
server api env = hoistServer
  api
  (runAppAsHandler env)
  (tokenAPI :<|> fulfillmentAPI)

app :: Env -> Application
app env = serve api $ server api env
  where
    api = Proxy :: Proxy API
