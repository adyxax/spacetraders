{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module SpaceTraders.Model.Nav
  ( Nav(..)
  ) where

import           Data.Aeson
import qualified Data.Text                as T
import           GHC.Generics

import           SpaceTraders.Model.Route

data Nav = Nav { flightMode     :: T.Text
               , route          :: Route
               , status         :: T.Text
               , systemSymbol   :: T.Text
               , waypointSymbol :: T.Text
               } deriving (FromJSON, Generic, Show, ToJSON)
