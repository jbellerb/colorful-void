-- Copyright (C) 2020  Jared Beller
-- This file is part of void-client
--
-- void-client is free software: you can redistribute it and/or modify
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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Token where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Data.Time
import GHC.Generics
import Servant
import Servant.Auth.Server
import Web.FormUrlEncoded
import qualified Auth as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M

expiry :: Int
expiry = 1 * 60 * 60

data TokenType = Refresh | Access
  deriving (Eq, Show, Generic)

instance ToJSON TokenType where
  toJSON Refresh = String "r"
  toJSON Access = String "a"

instance FromJSON TokenType where
  parseJSON (String "r") = return Refresh
  parseJSON (String "a") = return Access
  parseJSON _ = fail "kind must be r or a"

data Token = Token
  { kind :: TokenType
  , userID :: A.UserID
  } deriving (Eq, Show, Generic)

instance FromJSON Token where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJWT Token

instance ToJSON Token where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJWT Token

-- Token exchange API

type TokenAPI =
  "token" :> ReqBody '[FormUrlEncoded] TokenRequest :> Post '[JSON] TokenResponse

-- Exchange API handler

tokenAPI :: JWTSettings -> Server TokenAPI
tokenAPI jwts AuthExchange{..} = (=<<) handleError $ runMaybeT $ do
  guard(clientID == A.clientID)
  guard(clientSecret == A.clientSecret)
 
  userID <- MaybeT $ pure $ M.lookup code A.auths
  time <- liftIO getCurrentTime
  let expire = addUTCTime (realToFrac expiry) time
  accessToken <-
    unpackEither =<< (liftIO $ makeJWT (Token Access userID) jwts (Just expire))
  refreshToken <-
    unpackEither =<< (liftIO $ makeJWT (Token Refresh userID) jwts Nothing)

  return $
    TokenSuccess
      (decodeUtf8 $ BSL.toStrict accessToken)
      (Just $ decodeUtf8 $ BSL.toStrict refreshToken)
      expiry
tokenAPI jwts RefreshExchange{..} = (=<<) handleError $ runMaybeT $ do
  guard(clientID == A.clientID)
  guard(clientSecret == A.clientSecret)

  Token{..} <- MaybeT $ liftIO $ verifyJWT jwts $ BS.pack refreshToken
  guard(kind == Refresh)
  guard(M.member (userID) A.users)

  time <- liftIO getCurrentTime
  let expire = addUTCTime (realToFrac expiry) time
  accessToken <-
    unpackEither =<< (liftIO $ makeJWT (Token Access userID) jwts (Just expire))

  return $ TokenSuccess (decodeUtf8 $ BSL.toStrict accessToken) Nothing expiry

unpackEither :: (Monad m) => Either b a -> MaybeT m a
unpackEither = MaybeT . pure . either (const Nothing) Just

handleError :: Maybe a -> Handler a
handleError (Just x) = return x
handleError Nothing  = throwError $ err400 { errBody = encode TokenError }

-- Request types

data TokenRequest
  = AuthExchange
      { clientID :: String
      , clientSecret :: String
      , code :: String
      , redirectURI :: String
      }
  | RefreshExchange
      { clientID :: String
      , clientSecret :: String
      , refreshToken :: String
      }
  deriving (Show)

instance FromForm TokenRequest where
  fromForm f = do
    kind <- parseUnique "grant_type" f :: Either Text Text
    case kind of
      "authorization_code" ->
        AuthExchange
          <$> parseUnique "client_id" f
          <*> parseUnique "client_secret" f
          <*> parseUnique "code" f
          <*> parseUnique "redirect_uri" f
      "refresh_token" ->
        RefreshExchange
          <$> parseUnique "client_id" f
          <*> parseUnique "client_secret" f
          <*> parseUnique "refresh_token" f
      _ -> fail "grant_type must be authorization_code or refresh_token"

data TokenResponse
  = TokenError
  | TokenSuccess
      { accessToken :: Text
      , refreshToken :: Maybe Text
      , expiresIn :: Int
      }
  deriving (Show)

instance ToJSON TokenResponse where
  toJSON TokenError =
    object [ "error" .= ("invalid_grant" :: String) ]
  toJSON TokenSuccess{..} = object $
    [ "token_type" .= ("Bearer" :: String)
    , "access_token" .= accessToken
    , "expires_in" .= expiresIn
    ] ++ (maybe [] (\a -> ["refresh_token" .= a]) refreshToken)
