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

data APIError = APIError { apiErrorCode :: Int
                         , apiErrorData :: Value
                         , apiErrorMessage :: T.Text
                         } deriving Show
instance Exception APIError
instance FromJSON APIError where
  parseJSON (Object o) = do
    e <- o .: "error"
    APIError <$> e .: "code"
             <*> e .: "data"
             <*> e .: "message"
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
