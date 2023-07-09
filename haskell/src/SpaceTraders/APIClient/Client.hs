{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.APIClient.Client
  ( APIError(..)
  , APIMessage(..)
  , defaultReq
  , fromJSONValue
  , send
  , tokenReq
  ) where

import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Simple
import Network.HTTP.Types.Status

import SpaceTraders.APIClient.Errors

data FromJSON a => APIMessage a = APIMessage { data_ :: a } deriving (Show)
instance FromJSON a => FromJSON (APIMessage a) where
  parseJSON (Object o) = APIMessage <$> o .: "data"
  parseJSON _ = mzero

defaultReq :: Request
defaultReq = setRequestHost "api.spacetraders.io"
           $ setRequestPort 443
           $ setRequestSecure True
           $ setRequestHeader "Content-Type" ["application/json"]
           $ defaultRequest

tokenReq :: T.Text -> Request
tokenReq token = setRequestHeader "Authorization" [T.encodeUtf8 $ "Bearer " <> token]
               $ defaultReq

fromJSONValue :: FromJSON a => Value -> Either String a
fromJSONValue = parseEither parseJSON

send :: FromJSON a => Request -> IO (Either APIError a)
send request = do
  response <- httpLbs request
  let status = statusCode $ getResponseStatus response
      body = getResponseBody response
  if status >= 200 && status <= 299
    then case eitherDecode body of
      Left e -> return . Left $ APIError (-1000) (T.pack $ concat ["Error decoding JSON APIMessage: ", e]) Null
      Right r -> return . Right $ data_ r
    else case eitherDecode body of
      Left e -> return . Left $ APIError (-status) (T.pack $ concat ["Error decoding JSON APIError: ", e, ". Got HTTP body: ", show body]) Null
      Right (APIRateLimit r) -> do
        threadDelay (1_000_000 * (round $ retryAfter r))
        send request
      Right e -> return $ Left e
