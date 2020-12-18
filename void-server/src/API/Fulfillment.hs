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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module API.Fulfillment where

import API.Fulfillment.Execute
import API.Fulfillment.Query
import API.Fulfillment.Sync
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text
import GHC.Generics
import Servant
import Servant.Auth.Server
import qualified Auth as A
import qualified Data.Map as M

type FulfillmentAPI =
  "fulfillment"
    :> ReqBody '[JSON] FulfillmentRequest
    :> Post '[JSON] FulfillmentResponse

-- Fulfillment API handler

fulfillmentAPI :: JWTSettings -> Server FulfillmentAPI
fulfillmentAPI jwts FulfillmentRequest{..} = undefined

-- Request/response types

data FulfillmentRequest = FulfillmentRequest
  { requestId :: String
  , inputs :: [Intent]
  } deriving (Show, Generic, FromJSON)

data Intent
  = SyncIntent
  | QueryIntent { devices :: [A.DeviceID] }
  | ExecuteIntent { commands :: [Command] }
  deriving (Show)

instance FromJSON Intent where
  parseJSON = withObject "intent" $ \o -> do
    intentType <- o .: "intent" :: Parser Text
    case intentType of
      "action.devices.SYNC" -> return SyncIntent
      "action.devices.QUERY" -> do
        payload <- o .: "payload"
        devices <- payload .: "devices"
        return QueryIntent{..}
      "action.devices.EXECUTE" -> do
        payload <- o .: "payload"
        commands <- payload .: "commands"
        return ExecuteIntent{..}
      _ -> fail ("unknown intent type: " ++ (show intentType))

data FulfillmentResponse = FulfillmentResponse
  { requestId :: String
  , payload :: [IntentResponse]
  } deriving (Show, Generic, ToJSON)

data IntentResponse
  = SyncResponse { agentUserId :: String, deviceSpecs :: [DeviceSpec] }
  | QueryResponse { deviceStatuses :: M.Map String DeviceStatus }
  | ExecuteResponse { commands :: [CommandResult] }
  deriving (Show)

instance ToJSON IntentResponse where
  toJSON SyncResponse{..} =
    object [ "agentUserID" .= agentUserId, "devices" .= deviceSpecs ]
  toJSON QueryResponse{..} = object [ "devices" .= deviceStatuses ]
  toJSON ExecuteResponse{..} = object [ "commands" .= commands ]
