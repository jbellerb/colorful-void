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

module Main where

import API.Client
import API.Server
import API.Token
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server

main :: IO ()
main = do
  myKey <- generateKey
  let jwts = defaultJWTSettings myKey
      api = Proxy :: Proxy API
  run 8080 $ serve api (tokenAPI jwts)
