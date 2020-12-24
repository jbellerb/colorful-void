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

{-# LANGUAGE RecordWildCards #-}

module Main where

import App
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Logger (withStdoutLogger)
import Servant.Auth.Server
import Servant.Server
import Server

mkEnv :: IO Env
mkEnv = do
  jwtKey <- generateKey
  manager <- newManager defaultManagerSettings
  let jwts = defaultJWTSettings jwtKey

  return Env{..}

runServer :: Env -> IO ()
runServer env = withStdoutLogger $ \logger -> do
  let settings = setPort 8080 $ setLogger logger defaultSettings
      cfg = defaultCookieSettings :. (jwts env) :. EmptyContext
  runSettings settings $ app cfg env

main :: IO ()
main = mkEnv >>= runServer
