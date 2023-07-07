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
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Simple
import Network.HTTP.Types.Status

import SpaceTraders.APIClient.Errors

data APIMessage = APIMessage { data_ :: Value } deriving (Show)
instance FromJSON APIMessage where
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
      Left e -> return . Left $ APIError (-1000) Null (T.pack $ concat ["Error decoding JSON APIMessage: ", e])
      Right r -> case fromJSONValue (data_ r) of
        Left e -> return . Left $ APIError (-1001) Null (T.pack $ concat ["Error decoding JSON message contents: ", e])
        Right m -> return $ Right m
    else case eitherDecode body of
      Left e -> return . Left $ APIError (-status) Null (T.pack $ concat ["Error decoding JSON APIError: ", e, ". Got HTTP body: ", show body])
      Right e -> case apiErrorCode e of
        429 -> do -- We are being rate limited
          let d = apiErrorData e
          w <- case fromJSONValue d of
            Left _ -> throwIO e
            Right e' -> return $ retryAfter e'
          threadDelay (1_000_000 * (round w))
          send request
        _ -> return $ Left e

--handleAPIError :: SomeException -> IO (Maybe RegisterMessage)
--handleAPIError e = do
--  print e
--  return Nothing
