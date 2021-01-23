{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  API.Fulfillment.Query
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

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
