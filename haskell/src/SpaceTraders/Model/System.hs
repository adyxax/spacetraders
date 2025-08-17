module SpaceTraders.Model.System
  ( System(..)
  , Waypoint(..)
  ) where

import           Data.Aeson
import qualified Data.Text    as T
import           GHC.Generics

data System = System
  { sectorSymbol :: T.Text
  , symbol       :: T.Text
  , type_        :: T.Text
  , x            :: Int
  , y            :: Int
  , waypoints    :: [Waypoint]
  --, factions :: [Faction]
  } deriving (Generic, Show)

instance FromJSON System where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \x -> if x == "type_" then "type" else x }

data Waypoint = Waypoint { orbits :: Maybe T.Text
                         , symbol :: T.Text
                         , type_  :: T.Text
                         , x      :: Int
                         , y      :: Int
                         } deriving (Generic, Show)

instance FromJSON Waypoint where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \x -> if x == "type_" then "type" else x }
