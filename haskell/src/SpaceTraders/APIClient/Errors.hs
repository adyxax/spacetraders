{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.APIClient.Errors
  ( APIError(..)
  , RateLimit(..)
  , ResetHappened(..)
  ) where

import Control.Exception
import Data.Aeson
import Data.Time
import qualified Data.Text as T
import GHC.Generics

data APIError = APIError Int T.Text Value
              | APIRateLimit RateLimit
              | APIResetHappened ResetHappened
              deriving Show
instance Exception APIError
instance FromJSON APIError where
  parseJSON = withObject "APIError" $ \o -> do
    e <- o .: "error"
    code <- e .: "code"
    d <- e .: "data"
    case code of
      401 -> APIResetHappened <$> parseJSON d
      429 -> APIRateLimit <$> parseJSON d
      _ -> APIError <$> pure code
                    <*> e .: "message"
                    <*> pure d

data RateLimit = RateLimit { limitBurst :: Int
                           , limitPerSecond :: Int
                           , rateLimitType :: T.Text
                           , remaining :: Int
                           , reset :: UTCTime
                           , retryAfter :: Double
                           } deriving Show
instance FromJSON RateLimit where
  parseJSON = withObject "RateLimit" $ \o ->
    RateLimit <$> o .: "limitBurst"
              <*> o .: "limitPerSecond"
              <*> o .: "type"
              <*> o .: "remaining"
              <*> o .: "reset"
              <*> o .: "retryAfter"

data ResetHappened = ResetHappened { actual :: T.Text
                                   , expected :: T.Text
                                   } deriving (FromJSON, Generic, Show, ToJSON)
