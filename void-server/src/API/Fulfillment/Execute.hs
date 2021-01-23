{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  API.Fulfillment.Execute
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module API.Fulfillment.Execute where

import API.Client
import App
import Auth
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.List (concat, foldl')
import Data.Text (Text)
import GHC.Generics
import Servant
import Servant.Client
import qualified Data.Map as M

handleExecute :: UserID -> [Command] -> App [CommandResult]
handleExecute userID = fmap concat . mapM (executeBulkCommand userID)

executeBulkCommand :: UserID -> Command -> App [CommandResult]
executeBulkCommand userID Command{..} = do
  devices <- mapM (identifyDevice userID) devices
  results <- sequence $ do
    (deviceID, device) <- devices
    job <- execution
    return $ executeSingleCommand deviceID device job
  return $ foldl' groupResults [] results

identifyDevice :: UserID -> DeviceID -> App (DeviceID, Device)
identifyDevice userID deviceID = do
  device@Device{..} <- case M.lookup deviceID Auth.devices of
    Just a -> return a
    Nothing -> throwError err401
  unless (owner == userID) $ throwError err401
  return (deviceID, device)

executeSingleCommand :: DeviceID -> Device -> CommandJob -> App CommandResult
executeSingleCommand deviceID Device{..} JobOnOff{..} = do
  Env{manager} <- ask
  let endpoint = setActive $ ActiveRequest active
  res <- liftIO $ runClientM endpoint (mkClientEnv manager address)
  case res of
    Left err -> return $ CommandOffline [deviceID]
    Right ActiveRequest{..} -> return $ OnOffSuccess [deviceID] value
executeSingleCommand deviceID Device{..} JobBrightnessAbsolute{..} = do
  Env{manager} <- ask
  let endpoint = setBrightness $ BrightnessRequest brightness
  res <- liftIO $ runClientM endpoint (mkClientEnv manager address)
  case res of
    Left err -> return $ CommandOffline [deviceID]
    Right BrightnessRequest{..} -> return $ BrightnessSuccess [deviceID] value
executeSingleCommand deviceID Device{..} JobColorAbsolute{..} = do
  Env{manager} <- ask
  let endpoint = setColor $ ColorRequest color
  res <- liftIO $ runClientM endpoint (mkClientEnv manager address)
  case res of
    Left err -> return $ CommandOffline [deviceID]
    Right ColorRequest{..} -> return $ ColorSuccess [deviceID] value

groupResults :: [CommandResult] -> CommandResult -> [CommandResult]
groupResults [] result = [result]
groupResults (g:gs) result
  | resultCommon g result = mergeIDs g result : gs
  | otherwise = g : groupResults gs result

resultCommon :: CommandResult -> CommandResult -> Bool
resultCommon CommandOffline{} CommandOffline{} = True
resultCommon OnOffSuccess{active = a} OnOffSuccess{active = b} = a == b
resultCommon
  BrightnessSuccess{brightness = a}
  BrightnessSuccess{brightness = b} = a == b
resultCommon ColorSuccess{color = a} ColorSuccess{color = b} = a == b
resultCommon _ _ = False

mergeIDs :: CommandResult -> CommandResult -> CommandResult
mergeIDs
 CommandOffline{ids = a}
 CommandOffline{ids = b} = CommandOffline{ids = a ++ b}
mergeIDs
  OnOffSuccess{ids = a, ..}
  OnOffSuccess{ids = b} = OnOffSuccess{ids = a ++ b, ..}
mergeIDs
  BrightnessSuccess{ids = a, ..}
  BrightnessSuccess{ids = b} = BrightnessSuccess{ids = a ++ b, ..}
mergeIDs
 ColorSuccess{ids = a, ..}
 ColorSuccess{ids = b} = ColorSuccess{ids = a ++ b, ..}
 
-- Request/response types

data Command = Command { devices :: [DeviceID], execution :: [CommandJob] }
  deriving (Show, Generic, FromJSON)

data CommandJob
  = JobOnOff { active :: Bool }
  | JobBrightnessAbsolute { brightness :: Int }
  | JobColorAbsolute { color :: Int }
  deriving (Show)

instance FromJSON CommandJob where
  parseJSON = withObject "command job" $ \o -> do
    command <- o .: "command" :: Parser Text
    params <- o .: "params"
    case command of
      "action.devices.commands.OnOff" -> JobOnOff <$> params .: "on"
      "action.devices.commands.BrightnessAbsolute" ->
        JobBrightnessAbsolute <$> params .: "brightness"
      "action.devices.commands.ColorAbsolute" -> do
        colorInfo <- params .: "color"
        color <- colorInfo .: "spectrumRGB"
        return JobColorAbsolute{..}
      _ -> fail ("unknown command type: " ++ show command)

data CommandResult
  = CommandOffline { ids :: [DeviceID] }
  | OnOffSuccess { ids :: [DeviceID], active :: Bool }
  | BrightnessSuccess { ids :: [DeviceID], brightness :: Int }
  | ColorSuccess { ids :: [DeviceID], color :: Int }
  deriving (Show)

instance ToJSON CommandResult where
  toJSON CommandOffline{..} = object
    [ "ids" .= map getDeviceID ids
    , "status" .= ("OFFLINE" :: Text)
    ]
  toJSON OnOffSuccess{..} = object
    [ "ids" .= map getDeviceID ids
    , "status" .= ("SUCCESS" :: Text)
    , "states" .= object [ "on" .= active, "online" .= True ]
    ]
  toJSON BrightnessSuccess{..} = object
    [ "ids" .= map getDeviceID ids
    , "status" .= ("SUCCESS" :: Text)
    , "states" .= object [ "brightness" .= brightness, "online" .= True ]
    ]
  toJSON ColorSuccess{..} = object
    [ "ids" .= map getDeviceID ids
    , "status" .= ("SUCCESS" :: Text)
    , "states" .= object
        [ "color" .= object [ "spectrumRgb" .= color ]
        , "online" .= True ]
    ]
