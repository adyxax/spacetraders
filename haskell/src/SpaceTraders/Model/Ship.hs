{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Model.Ship
  ( Ship(..)
  , isDocked
  , isInOrbit
  ) where

import           Data.Aeson
import qualified Data.Text                as T
import           GHC.Generics

import           SpaceTraders.Model.Cargo
import           SpaceTraders.Model.Fuel
import           SpaceTraders.Model.Nav

data Ship = Ship { cargo    :: Cargo
                 , cooldown :: Cooldown
                 --, crew :: Crew
                 --, engine :: Engine
                 --, frame :: Frame
                 , fuel     :: Fuel
                 --, modules :: [Module]
                 --, mounts :: [Mount]
                 , nav      :: Nav
                 --, reactor :: Reactor
                 --, registration :: Registration
                 , symbol   :: T.Text
                 } deriving (FromJSON, Generic, Show, ToJSON)

data Cooldown = Cooldown { shipSymbol       :: T.Text
                         , totalSeconds     :: Int
                         , remainingSeconds :: Int
                         } deriving (FromJSON, Generic, Show, ToJSON)

isDocked :: Ship -> Bool
isDocked ship = status (nav ship) == "DOCKED"

isInOrbit :: Ship -> Bool
isInOrbit = not . isDocked
