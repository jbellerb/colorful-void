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

module API.Fulfillment.Execute where

import Auth
import Data.Aeson
import Data.Text hiding (map)
import GHC.Generics

data Command = Command { devices :: [A.DeviceID], execution :: [CommandJob] }
  deriving (Show, Generic, FromJSON)

data CommandJob = CommandJob { command :: String }
  deriving (Show, Generic, FromJSON)

data CommandResult
  = CommandOffline { ids :: [DeviceID] }
  | OnOffSuccess { ids :: [DeviceID], active :: Bool }
  | BrightnessSuccess { ids :: [DeviceID], brightness :: Int }
  | ColorSuccess { ids :: [DeviceID], color :: Int }
  deriving (Show)

instance ToJSON CommandResult where
  toJSON CommandOffline{..} = object
    [ "ids" .= map (\DeviceID{..} -> deviceID) ids
    , "status" .= ("OFFLINE" :: Text)
    ]
  toJSON OnOffSuccess{..} = object
    [ "ids" .= map (\DeviceID{..} -> deviceID) ids
    , "status" .= ("SUCCESS" :: Text)
    , "states" .= object [ "on" .= active, "online" .= True ]
    ]
  toJSON BrightnessSuccess{..} = object
    [ "ids" .= map (\DeviceID{..} -> deviceID) ids
    , "status" .= ("SUCCESS" :: Text)
    , "states" .= object [ "brightness" .= brightness, "online" .= True ]
    ]
  toJSON ColorSuccess{..} = object
    [ "ids" .= map (\DeviceID{..} -> deviceID) ids
    , "status" .= ("SUCCESS" :: Text)
    , "states" .= object
        [ "color" .= (object [ "spectrumRgb" .= color ])
        , "online" .= True ]
    ]
