{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.APIClient.Client
  ( APIError(..)
  , APIMessage(..)
  , APIPaginatedResponse
  , APIResponse
  , defaultReq
  , fromJSONValue
  , paginatedReq
  , send
  , sendPaginated
  , tokenReq
  ) where

import Control.Concurrent
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import System.Environment
import System.Posix.Process

import SpaceTraders.APIClient.Errors
import SpaceTraders.APIClient.Pagination

data FromJSON a => APIMessage a = APIMessage { messageData :: a
                                             , messagePagination :: Maybe Pagination
                                             } deriving (Show)
instance FromJSON a => FromJSON (APIMessage a) where
  parseJSON = withObject "APIMessage" $ \o ->
    APIMessage <$> o .: "data"
               <*> o .:? "meta"

type APIPaginatedResponse a = Either APIError (APIMessage a)
type APIResponse a = Either APIError a

defaultReq :: Request
defaultReq = setRequestHost "api.spacetraders.io"
           $ setRequestPort 443
           $ setRequestSecure True
           $ setRequestHeader "Content-Type" ["application/json"]
           $ defaultRequest

paginatedReq :: T.Text -> Maybe Pagination -> Request
paginatedReq token Nothing = setRequestQueryString [("limit", Just "20")]
                           $ tokenReq token
paginatedReq token (Just myPage) = setRequestQueryString [("limit", Just . int2ByteString $ limit myPage), ("page", Just . int2ByteString $ page myPage)]
                                 $ tokenReq token
  where
    int2ByteString = B.pack . map B.c2w . show

tokenReq :: T.Text -> Request
tokenReq token = setRequestHeader "Authorization" [T.encodeUtf8 $ "Bearer " <> token]
               $ defaultReq

fromJSONValue :: FromJSON a => Value -> Either String a
fromJSONValue = parseEither parseJSON

send :: FromJSON a => Request -> IO (APIResponse a)
send request = do
  response <- sendPaginated request
  case response of
    Left e -> return $ Left e
    Right (APIMessage d _) -> return $ Right d

sendPaginated :: FromJSON a => Request -> IO (APIPaginatedResponse a)
sendPaginated request = do
  response <- httpLbs request
  let status = statusCode $ getResponseStatus response
      body = getResponseBody response
  if status >= 200 && status <= 299
    then case eitherDecode body of
      Left e -> return . Left $ APIError (-1000) (T.pack $ concat ["Error decoding JSON APIMessage: ", e]) Null
      Right r -> return $ Right r
    else case eitherDecode body of
      Left e -> return . Left $ APIError (-status) (T.pack $ concat ["Error decoding JSON APIError: ", e, ". Got HTTP body: ", show body]) Null
      Right (APIRateLimit r) -> do
        threadDelay (1_000_000 * (round $ retryAfter r))
        sendPaginated request
      Right (APIResetHappened _) -> do
        p <- getExecutablePath
        a <- getArgs
        e <- getEnvironment
        executeFile p False a (Just e)
      Right e -> return $ Left e
