module SpaceTraders.ApiClient.Client
  ( ApiResponse
  , defaultReq
  , send
  , sendPaginated
  , tokenReq
  ) where

import           Control.Concurrent.Thread.Delay
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

-- HTTP request objects --------------------------------------------------------
defaultReq :: Request
defaultReq = setRequestHost "api.spacetraders.io"
           $ setRequestPort 443
           $ setRequestSecure True
           $ setRequestHeader "Content-Type" ["application/json"]
             defaultRequest

tokenReq :: T.Text -> Request
tokenReq token = setRequestHeader "Authorization" [T.encodeUtf8 $ "Bearer " <> token] defaultReq

-- Sending HTTP requests -------------------------------------------------------
data SuccessResponse a = SuccessResponse
  { data_ :: a
  , meta  :: Maybe Pagination
  } deriving (Generic, Show)

instance FromJSON a => FromJSON (SuccessResponse a) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \x -> if x == "data_" then "data" else x }

type ApiResponse a = Either ApiError a

send :: FromJSON a => (Request -> Request) -> SpaceTradersT (ApiResponse a)
send requestBuilder = do
  env <- ask
  let request = requestBuilder env.request
  response <- sendAndDecode request
  pure $ case response of
    Left e  -> Left e
    Right r -> Right r.data_

sendPaginated :: (FromJSON a, Semigroup a) => Pagination -> (Request -> Request) -> SpaceTradersT (ApiResponse a)
sendPaginated pagination requestBuilder = do
  env <- ask
  let request = requestBuilder env.request
  response <- sendAndDecode request
  case response of
    Left e  -> pure $ Left e
    Right r -> if isLastPage r.meta then pure (Right r.data_)
                                    else do
      response' <- sendPaginated (nextPage pagination) requestBuilder
      pure $ case response' of
        Left e  -> Left e
        Right d -> Right $ r.data_ <> d

sendAndDecode :: FromJSON a => Request -> SpaceTradersT (ApiResponse (SuccessResponse a))
sendAndDecode request = do
   response <- liftIO $ httpLbs request
   let status = Network.HTTP.Types.Status.statusCode $ getResponseStatus response
       body = getResponseBody response
   if status >= 200 && status <= 299
     then case eitherDecode body of
       Left e  -> pure . Left $ ApiBodyDecodeError status (T.pack e) (TL.toStrict $ TL.decodeUtf8 body)
       Right r -> pure $ Right r
     else case eitherDecode body of
       Left e -> pure . Left $ ApiBodyDecodeError status (T.pack e) (TL.toStrict $ TL.decodeUtf8 body)
       Right (ApiRateLimit r) -> do
         liftIO $ delay (1_000_000 * round r.retryAfter)
         sendAndDecode request
       Right e -> pure $ Left e

int2ByteString :: Int -> B.ByteString
int2ByteString = B.pack . map B.c2w . show
