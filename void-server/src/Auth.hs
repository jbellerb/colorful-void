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

module Auth where

import Data.Aeson
import GHC.Generics
import qualified Data.Map as M

data UserID = UserID Int
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data DeviceID = DeviceID { deviceID :: String }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data Device = Device { address :: String, name :: String }
  deriving (Eq, Show)

auths :: M.Map String UserID
auths = M.fromList [("auth 0", UserID 0)]

users :: M.Map UserID [DeviceID]
users = M.fromList [(UserID 0, [DeviceID "0"])]

devices :: M.Map DeviceID Device
devices = M.fromList [(DeviceID "0", Device "127.0.0.1" "Colorful Void")]

clientID :: String
clientID = "google"

clientSecret :: String
clientSecret = "client secret"
