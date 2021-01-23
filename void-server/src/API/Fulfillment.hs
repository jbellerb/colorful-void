{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      :  API.Fulfillment
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module API.Fulfillment where

import App
import API.Fulfillment.Execute
import API.Fulfillment.Query
import API.Fulfillment.Sync
import Control.Monad.IO.Class
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

fulfillmentAPI :: A.UserID -> ServerT FulfillmentAPI App
fulfillmentAPI userID FulfillmentRequest{inputs = intent : _, ..} = do
  response <- case intent of
    SyncIntent -> SyncResponse userID <$> handleSync userID
    QueryIntent{..} -> QueryResponse <$> handleQuery userID devices
    ExecuteIntent{..} -> ExecuteResponse <$> handleExecute userID commands
  return $ FulfillmentResponse requestId response
fulfillmentAPI _ _ = throwError $
  err400 { errBody = "Error in $.inputs: no intent provided" }

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
      _ -> fail ("unknown intent type: " ++ show intentType)

data FulfillmentResponse = FulfillmentResponse
  { requestId :: String
  , payload :: IntentResponse
  } deriving (Show, Generic, ToJSON)

data IntentResponse
  = SyncResponse { agentUserID :: A.UserID, deviceSpecs :: [DeviceSpec] }
  | QueryResponse { deviceStatuses :: M.Map String DeviceStatus }
  | ExecuteResponse { commands :: [CommandResult] }
  deriving (Show)

instance ToJSON IntentResponse where
  toJSON SyncResponse{..} =
    object [ "agentUserId" .= agentUserID, "devices" .= deviceSpecs ]
  toJSON QueryResponse{..} = object [ "devices" .= deviceStatuses ]
  toJSON ExecuteResponse{..} = object [ "commands" .= commands ]
