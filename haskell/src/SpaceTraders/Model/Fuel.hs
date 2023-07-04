{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Model.Fuel
  ( Consumed(..)
  , Fuel(..)
  ) where

import Data.Aeson
import Data.Time
import GHC.Generics

data Consumed = Consumed { amount :: Int
                         , timestamp :: UTCTime
                         } deriving (FromJSON, Generic, Show, ToJSON)

data Fuel = Fuel { capacity :: Int
                 , consumed :: Consumed
                 , current :: Int
                 } deriving (FromJSON, Generic, Show, ToJSON)
