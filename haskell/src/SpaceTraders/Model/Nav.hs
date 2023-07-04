{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Model.Nav
  ( Nav(..)
  ) where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

import SpaceTraders.Model.Route

data Nav = Nav { flightMode :: T.Text
               , route :: Route
               , status :: T.Text
               , systemSymbol :: T.Text
               , waypointSymbol :: T.Text
               } deriving (FromJSON, Generic, Show, ToJSON)
