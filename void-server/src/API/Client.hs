{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      :  API.Client
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module API.Client where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant
import Servant.Client

type ClientAPI =
  Get '[JSON] LightStatus :<|>
  "active" :> Get '[JSON] ActiveRequest :<|>
  "active" :> ReqBody '[JSON] ActiveRequest :> Put '[JSON] ActiveRequest :<|>
  "brightness" :> Get '[JSON] BrightnessRequest :<|>
  "brightness" :> ReqBody '[JSON] BrightnessRequest :> Put '[JSON] BrightnessRequest :<|>
  "color" :> Get '[JSON] ColorRequest :<|>
  "color" :> ReqBody '[JSON] ColorRequest :> Put '[JSON] ColorRequest

-- Client API handler

clientAPI :: Proxy ClientAPI
clientAPI = Proxy

getStatus :<|> getActive :<|> setActive :<|> getBrightness :<|> setBrightness :<|> getColor :<|> setColor = client clientAPI

-- Request/response types

data LightStatus = LightStatus
  { active :: Bool
  , brightness :: Int
  , color :: Int
  } deriving (Show, Generic, FromJSON)

newtype ActiveRequest = ActiveRequest { value :: Bool }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype BrightnessRequest = BrightnessRequest { value :: Int }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype ColorRequest = ColorRequest { value :: Int }
  deriving (Show, Generic, FromJSON, ToJSON)
