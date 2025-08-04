module SpaceTraders.Model.Cargo
  ( Cargo(..)
  ) where

import           Data.Aeson
import           GHC.Generics

import           SpaceTraders.Model.Inventory (Inventory)

data Cargo = Cargo { capacity  :: Int
                   , inventory :: [Inventory]
                   , units     :: Int
                   } deriving (FromJSON, Generic, Show, ToJSON)
