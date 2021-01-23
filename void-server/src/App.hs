{- |
Module      :  App
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module App where

import Control.Monad.Trans.Reader (ReaderT)
import Network.HTTP.Client (Manager)
import Servant
import Servant.Auth.Server

data Env = Env
  { jwts :: JWTSettings
  , manager :: Manager
  }

type App = ReaderT Env Handler
