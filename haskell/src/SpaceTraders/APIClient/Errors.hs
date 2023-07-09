{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.APIClient.Errors
  ( APIError(..)
  , RateLimit(..)
  ) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Time
import qualified Data.Text as T

data APIError = APIError Int T.Text Value
              | APIRateLimit RateLimit
              deriving Show
instance Exception APIError
instance FromJSON APIError where
  parseJSON (Object o) = do
    e <- o .: "error"
    code <- e .: "code"
    d <- e .: "data"
    case code of
      429 -> APIRateLimit <$> parseJSON d
      _ -> APIError <$> pure code
                    <*> e .: "message"
                    <*> pure d
  parseJSON _ = mzero

data RateLimit = RateLimit { limitBurst :: Int
                           , limitPerSecond :: Int
                           , rateLimitType :: T.Text
                           , remaining :: Int
                           , reset :: UTCTime
                           , retryAfter :: Double
                           } deriving Show
instance FromJSON RateLimit where
  parseJSON (Object o) = do
    RateLimit <$> o .: "limitBurst"
              <*> o .: "limitPerSecond"
              <*> o .: "type"
              <*> o .: "remaining"
              <*> o .: "reset"
              <*> o .: "retryAfter"
  parseJSON _ = mzero
