module SpaceTraders.ApiClient.Errors
  ( ApiError(..)
  , RateLimit(..)
  , ResetHappened(..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception
import           Data.Aeson
import           Data.Text
import           Data.Time
import           GHC.Generics

data ApiError = ApiBodyDecodeError { statusCode :: Int, message :: Text, body :: Text }
              | ApiHTTPError HTTPError
              | ApiRateLimit RateLimit
              | ApiResetHappened ResetHappened
              | ApiResponseError ResponseError
              deriving (Generic, Show)

instance Exception ApiError
instance FromJSON ApiError where
  parseJSON v = withObject "ApiError" (\o ->
    ApiHTTPError <$> parseJSON v
    <|> (do
        e <- o .: "error"
        c <- e .: "code"
        d <- e .: "data"
        case c of
          429 -> ApiRateLimit <$> parseJSON d
          4113 -> ApiResetHappened <$> parseJSON d
          _ -> do
            m <- e .: "message"
            pure $ ApiResponseError $ ResponseError c m d)
    ) v
instance ToJSON ApiError

data HTTPError = HTTPError { error      :: Text
                           , message    :: Text
                           , statusCode :: Int
                           } deriving (Generic, Show)
instance FromJSON HTTPError
instance ToJSON HTTPError

data RateLimit = RateLimit { limitBurst     :: Int
                           , limitPerSecond :: Int
                           , remaining      :: Int
                           , reset          :: UTCTime
                           , retryAfter     :: Double
                           , type_          :: Text
                           } deriving (Generic, Show)

instance FromJSON RateLimit where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \x -> if x == "type_" then "type" else x }
instance ToJSON RateLimit where
  toJSON     = genericToJSON defaultOptions
    { fieldLabelModifier = \x -> if x == "type_" then "type" else x }

data ResetHappened = ResetHappened { actual   :: Text
                                   , expected :: Text
                                   } deriving (Generic, Show)
instance Exception ResetHappened
instance FromJSON ResetHappened
instance ToJSON ResetHappened

data ResponseError = ResponseError { statusCode :: Int
                                   , message    :: Text
                                   , data_      :: Value
                                   } deriving (Generic, Show)
instance FromJSON ResponseError
instance ToJSON ResponseError
