{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.APIClient.Client
  ( APIMessage(..)
  , APIPaginatedResponse
  , APIResponse
  , defaultReq
  , send
  , sendPaginated
  , tokenReq
  ) where

import Control.Concurrent.Thread.Delay
import Control.Monad
import Control.Monad.Reader
import Data.Aeson
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import System.Directory
import System.Environment
import System.Posix.Process

import SpaceTraders
import SpaceTraders.APIClient.Errors
import SpaceTraders.APIClient.Pagination
import SpaceTraders.Utils

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

tokenReq :: T.Text -> Request
tokenReq token = setRequestHeader "Authorization" [T.encodeUtf8 $ "Bearer " <> token] defaultReq

send :: (FromJSON a, HasRequest env, MonadIO m, MonadReader env m) => (Request -> Request) -> m (APIResponse a)
send requestBuilder = do
  response <- sendPaginated Nothing requestBuilder
  case response of
    Left e -> return $ Left e
    Right (APIMessage d _) -> return $ Right d

sendPaginated :: (FromJSON a, HasRequest env, MonadIO m, MonadReader env m) => Maybe Pagination -> (Request -> Request) -> m (APIPaginatedResponse a)
sendPaginated pagination requestBuilder = do
  env <- ask
  let request = requestBuilder $ getRequest env
      request' = case pagination of
        Just myPage -> setRequestQueryString [("limit", Just . int2ByteString $ limit myPage), ("page", Just . int2ByteString $ page myPage)]
                                           $ request
        Nothing -> request
  sendPaginated' request'
  where
    sendPaginated' :: (FromJSON a, HasRequest env, MonadIO m, MonadReader env m) => Request -> m (APIPaginatedResponse a)
    sendPaginated' request = do
      env <- ask
      let iref = getLastAPICall env
      response <- liftIO $ do
        now <- microSinceEpoch
        prev <- readIORef iref
        let diff = prev + 400_000 - now
        when (diff > 0) (delay diff)
        writeIORef iref now
        httpLbs request
      let status = statusCode $ getResponseStatus response
          body = getResponseBody response
      if status >= 200 && status <= 299
        then case eitherDecode body of
          Left e -> return . Left $ APIError (-1000) (T.pack $ concat ["Error decoding JSON APIMessage: ", e]) Null
          Right r -> return $ Right r
        else case eitherDecode body of
          Left e -> return . Left $ APIError (-status) (T.pack $ concat ["Error decoding JSON APIError: ", e, ". Got HTTP body: ", show body]) Null
          Right (APIRateLimit r) -> do
            liftIO $ delay (1_000_000 * (round $ retryAfter r))
            sendPaginated' request
          Right (APIResetHappened _) -> liftIO $ do
            removeFile "spacetraders.db"
            p <- getExecutablePath
            a <- getArgs
            e <- getEnvironment
            executeFile p False a (Just e) -- we exec on ourselves
          Right e -> return $ Left e
