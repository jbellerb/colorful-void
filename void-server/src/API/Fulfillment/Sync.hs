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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API.Fulfillment.Sync where

import App
import Data.Aeson
import Data.Maybe
import Data.Text hiding (map)
import Servant
import qualified Auth as A
import qualified Data.Map as M

handleSync :: A.UserID -> App [DeviceSpec]
handleSync userID =
  return $ mapMaybe lookup $ M.findWithDefault [] userID A.users
  where
    lookup deviceID = do
      A.Device{..} <- M.lookup deviceID A.devices
      return $ DeviceSpec deviceID name

-- Request/response types

data DeviceSpec = DeviceSpec
  { deviceID :: A.DeviceID
  , name :: String
  } deriving (Show)

instance ToJSON DeviceSpec where
  toJSON DeviceSpec{..} = object
    [ "id" .= A.getDeviceID deviceID
    , "type" .= ("action.devices.types.LIGHT" :: Text)
    , "traits" .= (
        [ "action.devices.traits.OnOff"
        , "action.devices.traits.Brightness"
        , "action.devices.traits.ColorSetting"
        ] :: [Text])
    , "name" .= object
        [ "defaultNames" .= (["colorful void"] :: [Text])
        , "name" .= name
        ]
    , "willReportState" .= False
    , "attributes" .= object [ "colorModel" .= ("rgb" :: Text) ]
    , "deviceInfo" .= object
        [ "manufacturer" .= ("Jared Beller" :: Text)
        , "model" .= ("void" :: Text)
        , "hwVersion" .= ("1.0" :: Text)
        , "swVersion" .= ("0.1.0" :: Text)
        ]
    ]
