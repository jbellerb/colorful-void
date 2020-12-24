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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Auth where

import Data.Aeson
import GHC.Generics
import Servant.Client
import qualified Data.Map as M

newtype UserID = UserID String
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype DeviceID = DeviceID { getDeviceID :: String }
  deriving (Eq, Ord, Show)

instance FromJSON DeviceID where
  parseJSON = withObject "device id" $ \o -> DeviceID <$> o .: "id"

instance ToJSON DeviceID where
  toJSON DeviceID{..} =
    object [ "id" .= getDeviceID ]

data Device = Device { address :: BaseUrl, name :: String, owner :: UserID }
  deriving (Eq, Show)

auths :: M.Map String UserID
auths = M.fromList [("auth 0", UserID "0")]

users :: M.Map UserID [DeviceID]
users = M.fromList [(UserID "0", [DeviceID "0"])]

devices :: M.Map DeviceID Device
devices = M.fromList
  [ ( DeviceID "0",
      Device
        (BaseUrl Http "localhost" 8000 "")
        "void-01"
        (UserID "0")
    )
  ]

clientID :: String
clientID = "google"

clientSecret :: String
clientSecret = "client secret"
