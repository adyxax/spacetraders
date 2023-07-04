{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Model.Ship
  ( Ship(..)
  ) where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

import SpaceTraders.Model.Cargo
import SpaceTraders.Model.Fuel
import SpaceTraders.Model.Nav

data Ship = Ship { cargo :: Cargo
                 --, crew :: Crew
                 --, engine :: Engine
                 --, frame :: Frame
                 , fuel :: Fuel
                 --, modules :: [Module]
                 --, mounts :: [Mount]
                 , nav :: Nav
                 --, reactor :: Reactor
                 --, registration :: Registration
                 , symbol :: T.Text
                 } deriving (FromJSON, Generic, Show, ToJSON)
