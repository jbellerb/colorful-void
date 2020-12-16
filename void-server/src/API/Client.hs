-- Copyright (C) 2020  Jared Beller
-- This file is part of void-client
--
-- void-client is free software: you can redistribute it and/or modify
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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module API.Client where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant
import Servant.Client

type ClientAPI =
  Get '[JSON] StatusRequest :<|>
  "active" :> Get '[JSON] ActiveRequest :<|>
  "active" :> ReqBody '[JSON] ActiveRequest :> Put '[JSON] ActiveRequest :<|>
  "brightness" :> Get '[JSON] BrightnessRequest :<|>
  "brightness" :> ReqBody '[JSON] BrightnessRequest :> Put '[JSON] BrightnessRequest :<|>
  "color" :> Get '[JSON] ColorRequest :<|>
  "color" :> ReqBody '[JSON] ColorRequest :> Put '[JSON] ColorRequest

-- API handler

clientAPI :: Proxy ClientAPI
clientAPI = Proxy

getStatus :<|> getActive :<|> setActive :<|> getBrightness :<|> setBrightness :<|> getColor :<|> setColor = client clientAPI

-- Request types

data StatusRequest = StatusRequest
  { active :: Bool
  , brightness :: Int
  , color :: Int
  } deriving (Show, Generic, FromJSON)

data ActiveRequest = ActiveRequest { value :: Bool }
  deriving (Show, Generic, FromJSON, ToJSON)

data BrightnessRequest = BrightnessRequest { value :: Int }
  deriving (Show, Generic, FromJSON, ToJSON)

data ColorRequest = ColorRequest { value :: Int }
  deriving (Show, Generic, FromJSON, ToJSON)
