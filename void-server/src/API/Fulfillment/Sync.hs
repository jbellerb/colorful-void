{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  API.Fulfillment.Sync
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

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
