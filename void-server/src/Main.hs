{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Main
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

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
