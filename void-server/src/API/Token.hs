{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      :  API.Token
Copyright   :  (c) Jared Beller 2020-2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module API.Token where

import App
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (ask)
import Data.Aeson
import Data.Aeson.Types (Parser)
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

data Token
  = AccessToken { userID :: A.UserID }
  | RefreshToken { clientID :: String, userID :: A.UserID }
  deriving (Show)

instance FromJSON Token where
  parseJSON = withObject "access token or refresh token" $ \o -> do
    tokenType <- o .: "t" :: Parser Text
    case tokenType of
      "a" -> AccessToken <$> o .: "u"
      "r" -> RefreshToken <$> o .: "c" <*> o .: "u"
      _ -> fail ("unknown token type: " ++ show tokenType)

instance FromJWT Token

instance ToJSON Token where
  toJSON AccessToken{..} =
    object [ "t" .= ("a" :: Text), "u" .= userID ]
  toJSON RefreshToken{..} =
    object [ "t" .= ("r" :: Text), "c" .= clientID, "u" .= userID ]

instance ToJWT Token

-- Token exchange API

type TokenAPI =
  "token" :> ReqBody '[FormUrlEncoded] TokenRequest :> Post '[JSON] TokenResponse

-- Exchange API handler

tokenAPI :: ServerT TokenAPI App
tokenAPI AuthExchange{..} = (=<<) handleError $ runMaybeT $ do
  Env{jwts} <- lift ask
  guard (clientID == A.clientID)
  guard (clientSecret == A.clientSecret)
 
  userID <- MaybeT $ pure $ M.lookup code A.auths
  time <- liftIO getCurrentTime
  let expire = addUTCTime (realToFrac expiry) time
  accessToken <-
    unpackEither =<< liftIO (makeJWT (AccessToken userID) jwts (Just expire))
  refreshToken <-
    unpackEither =<< liftIO (makeJWT (RefreshToken clientID userID) jwts Nothing)

  return $
    TokenSuccess
      (decodeUtf8 $ BSL.toStrict accessToken)
      (Just $ decodeUtf8 $ BSL.toStrict refreshToken)
      expiry
tokenAPI RefreshExchange{..} = (=<<) handleError $ runMaybeT $ do
  Env{jwts} <- lift ask
  guard (clientID == A.clientID)
  guard (clientSecret == A.clientSecret)

  RefreshToken{clientID = tokenClientID, ..} <-
    MaybeT $ liftIO $ verifyJWT jwts $ BS.pack refreshToken
  guard (tokenClientID == clientID)
  guard (M.member userID A.users)

  time <- liftIO getCurrentTime
  let expire = addUTCTime (realToFrac expiry) time
  accessToken <-
    unpackEither =<< liftIO (makeJWT (AccessToken userID) jwts (Just expire))

  return $ TokenSuccess (decodeUtf8 $ BSL.toStrict accessToken) Nothing expiry

unpackEither :: (Monad m) => Either b a -> MaybeT m a
unpackEither = MaybeT . pure . either (const Nothing) Just

handleError :: Maybe a -> App a
handleError (Just x) = return x
handleError Nothing  = throwError $ err400 { errBody = encode TokenError }

-- Request/response types

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
      _ -> Left $ pack ("unknown grant_type: " ++ show kind)

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
    object [ "error" .= ("invalid_grant" :: Text) ]
  toJSON TokenSuccess{..} = object $
    [ "token_type" .= ("Bearer" :: Text)
    , "access_token" .= accessToken
    , "expires_in" .= expiresIn
    ] ++ maybe [] (\a -> ["refresh_token" .= a]) refreshToken
