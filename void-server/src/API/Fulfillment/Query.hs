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

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API.Fulfillment.Query where

import API.Client
import App
import Auth
import Control.Monad (guard, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson
import Data.Text hiding (map)
import Servant
import Servant.Client
import qualified Data.Map as M

handleQuery :: UserID -> [DeviceID] -> App (M.Map String DeviceStatus)
handleQuery userID = fmap M.fromList . mapM \deviceID -> do
  Device{..} <- case M.lookup deviceID devices of
    Just a -> return a
    Nothing -> throwError $ err401
  unless (owner == userID) $ throwError err401

  Env{manager} <- ask
  res <- liftIO $ runClientM getStatus (mkClientEnv manager address)
  case res of
    Left err -> return (getDeviceID deviceID, StatusOffline)
    Right status -> return (getDeviceID deviceID, StatusSuccess status)

-- Request/response types

data DeviceStatus
  = StatusSuccess { status :: LightStatus }
  | StatusOffline
  deriving (Show)

instance ToJSON DeviceStatus where
  toJSON StatusSuccess{ status = LightStatus{..} } = object
    [ "online" .= True
    , "status" .= ("SUCCESS" :: Text)
    , "on" .= active
    , "brightness" .= brightness
    , "color" .= object [ "spectrumRgb" .= color ]
    ]
  toJSON StatusOffline = object
    [ "online" .= False
    , "status" .= ("OFFLINE" :: Text)
    ]
