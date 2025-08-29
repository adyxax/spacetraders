module SpaceTraders.ApiClient.Client
  ( send
  , sendPaginated
  ) where

import           Control.Concurrent.Thread.Delay
import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Internal          as B
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified Data.Text.Lazy                    as TL
import qualified Data.Text.Lazy.Encoding           as TL
import           GHC.Generics
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status

import           SpaceTraders
import           SpaceTraders.ApiClient.Errors
import           SpaceTraders.ApiClient.Pagination

-- Sending HTTP requests -------------------------------------------------------
data SuccessResponse a = SuccessResponse
  { data_ :: a
  , meta  :: Maybe Pagination
  } deriving (Generic, Show)

instance FromJSON a => FromJSON (SuccessResponse a) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \x -> if x == "data_" then "data" else x }

send :: FromJSON a => (Request -> Request) -> SpaceTradersT a
send requestBuilder = do
  env <- ask
  let request = requestBuilder env.request
  response <- sendAndDecode request
  pure response.data_

sendPaginated :: (FromJSON a, Semigroup a) => Pagination -> (Request -> Request) -> SpaceTradersT a
sendPaginated pagination requestBuilder = do
  env <- ask
  let request = requestBuilder
              $ setRequestQueryString [ ("limit", Just . int2ByteString $ pagination.limit)
                                      , ("page", Just . int2ByteString $ pagination.page) ]
              $ env.request
  response <- sendAndDecode request
  if isLastPage response.meta then pure response.data_
                              else do
    response' <- sendPaginated (nextPage pagination) requestBuilder
    pure $ response.data_ <> response'

sendAndDecode :: FromJSON a => Request -> SpaceTradersT (SuccessResponse a)
sendAndDecode request = do
   response <- liftIO $ httpLbs request
   let status = Network.HTTP.Types.Status.statusCode $ getResponseStatus response
       body = getResponseBody response
   if status >= 200 && status <= 299
     then case eitherDecode body of
       Left e  -> throw $ ApiBodyDecodeError status (T.pack e) (TL.toStrict $ TL.decodeUtf8 body)
       Right r -> pure r
     else case eitherDecode body of
       Left e -> throw $ ApiBodyDecodeError status (T.pack e) (TL.toStrict $ TL.decodeUtf8 body)
       Right (ApiResetHappened e) -> throw e
       Right (ApiRateLimit r) -> do
         liftIO $ delay (1_000_000 * round r.retryAfter)
         sendAndDecode request
       Right e -> throw e

int2ByteString :: Int -> B.ByteString
int2ByteString = B.pack . map B.c2w . show
