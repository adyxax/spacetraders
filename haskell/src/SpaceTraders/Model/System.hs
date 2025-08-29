module SpaceTraders.Model.System
  ( filterShipyardWaypoints
  , System(..)
  , Waypoint(..)
  ) where

import           Data.Aeson
import qualified Data.List    as L
import qualified Data.Text    as T
import           Data.Time
import           GHC.Generics

filterShipyardWaypoints :: [Waypoint] -> [Waypoint]
filterShipyardWaypoints = L.filter hasShipyard
  where
    hasShipyard :: Waypoint -> Bool
    hasShipyard w = case w.traits of
      Nothing     -> False
      Just traits -> L.elem (WaypointTrait "SHIPYARD") traits

data System = System
  --{ constellation :: T.Text
  --, name          :: T.Text
  --, sectorSymbol  :: T.Text
  { symbol    :: T.Text
  --, type_     :: T.Text
  , x         :: Int
  , y         :: Int
  , waypoints :: [Waypoint]
  --, factions :: [Faction]
  } deriving (Generic, Show)

instance FromJSON System where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \x -> if x == "type_" then "type" else x }
instance ToJSON System where
  toJSON     = genericToJSON defaultOptions
    { fieldLabelModifier = \x -> if x == "type_" then "type" else x }

data Waypoint = Waypoint
  { chart               :: Maybe WaypointChart
  --, faction
  --, modifiers
  --, orbits       :: Maybe T.Text
  --, orbitals
  , isUnderConstruction :: Maybe Bool
  , symbol              :: T.Text
  --, systemSymbol        :: T.Text
  , traits              :: Maybe [WaypointTrait]
  --, type_               :: T.Text
  , x                   :: Int
  , y                   :: Int
  } deriving (Generic, Show)

instance FromJSON Waypoint where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \x -> if x == "type_" then "type" else x }
instance ToJSON Waypoint where
  toJSON     = genericToJSON defaultOptions
    { fieldLabelModifier = \x -> if x == "type_" then "type" else x
    , omitNothingFields = True }

data WaypointChart = WaypointChart
  { submittedBy    :: T.Text
  --, submittedOn    :: UTCTime
  --, waypointSymbol :: T.Text
  } deriving (Generic, Show)

instance FromJSON WaypointChart
instance ToJSON WaypointChart

data WaypointTrait = WaypointTrait
  --{ description
  --, name
  { symbol :: T.Text
  } deriving (Eq, Generic, Show)

instance FromJSON WaypointTrait
instance ToJSON WaypointTrait
