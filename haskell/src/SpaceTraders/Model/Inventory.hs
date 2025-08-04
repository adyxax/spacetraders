module SpaceTraders.Model.Inventory
  ( Inventory(..)
  ) where

import           Data.Aeson
import qualified Data.Text    as T
import           GHC.Generics

data Inventory = Inventory { description :: T.Text
                           , name        :: T.Text
                           , symbol      :: T.Text
                           , units       :: Int
                           } deriving (FromJSON, Generic, Show, ToJSON)
