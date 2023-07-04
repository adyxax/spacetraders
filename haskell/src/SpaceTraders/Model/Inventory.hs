{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Model.Inventory
  ( Inventory(..)
  ) where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

data Inventory = Inventory { description :: T.Text
                           , name :: T.Text
                           , symbol :: T.Text
                           , units :: Int
                           } deriving (FromJSON, Generic, Show, ToJSON)
