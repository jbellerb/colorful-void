{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Auth
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

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
