{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.APIClient.Client
  ( APIError(..)
  , APIMessage(..)
  , defaultReq
  , fromJSONValue
  , send
  , tokenReq
  ) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Simple
import Network.HTTP.Types.Status

data APIError = APIError { apiErrorCode :: Int
                         , apiErrorMessage :: T.Text
                         } deriving Show
instance Exception APIError
instance FromJSON APIError where
  parseJSON (Object o) = do
    e <- o .: "error"
    APIError <$> e .: "code"
             <*> e .: "message"
  parseJSON _ = mzero

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
      Left e -> return $ Left APIError{apiErrorCode = -1000, apiErrorMessage = T.pack $ concat ["Error decoding JSON APIMessage: ", e]}
      Right r -> case fromJSONValue (data_ r) of
        Left e -> return $ Left APIError{apiErrorCode = -1001, apiErrorMessage = T.pack $ concat ["Error decoding JSON message contents: ", e]}
        Right m -> return $ Right m
    else case eitherDecode body of
      Left e -> return $ Left APIError{apiErrorCode = -status, apiErrorMessage = T.pack $ concat ["Error decoding JSON APIError: ", e, ". Got HTTP body: ", show body]}
      Right e -> return $ Left e

--handleAPIError :: SomeException -> IO (Maybe RegisterMessage)
--handleAPIError e = do
--  print e
--  return Nothing
