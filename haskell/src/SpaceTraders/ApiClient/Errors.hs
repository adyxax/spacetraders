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
              deriving Show

instance Exception ApiError
instance FromJSON ApiError where
  parseJSON v = (ApiHTTPError <$> parseJSON v)
            <|> (ApiRateLimit <$> parseJSON v)
            <|> (ApiResetHappened <$> parseJSON v)
            <|> (ApiResponseError <$> parseJSON v)

data HTTPError = HTTPError { error      :: Text
                           , message    :: Text
                           , statusCode :: Int
                           } deriving (Generic, Show)
instance FromJSON HTTPError

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

data ResetHappened = ResetHappened { actual   :: Text
                                   , expected :: Text
                                   } deriving (Generic, Show)
instance FromJSON ResetHappened

data ResponseError = ResponseError { statusCode :: Int
                                   , message    :: Text
                                   , data_      :: Value
                                   } deriving (Generic, Show)
instance FromJSON ResponseError
