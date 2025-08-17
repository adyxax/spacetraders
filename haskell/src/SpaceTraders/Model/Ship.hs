module SpaceTraders.Model.Ship
  ( Cargo(..)
  , CargoItem(..)
  , Fuel(..)
  , FuelConsumed(..)
  , isDocked
  , isOrbitting
  , Nav(..)
  , Route(..)
  , RouteEndpoint(..)
  , Ship(..)
  ) where

import           Data.Aeson
import qualified Data.Text    as T
import           Data.Time
import           GHC.Generics

data Cargo = Cargo
  { capacity  :: Int
  , inventory :: [CargoItem]
  , units     :: Int
  } deriving (Generic, Show)

instance FromJSON Cargo

data CargoItem = CargoItem
  { description :: T.Text
  , name        :: T.Text
  , symbol      :: T.Text
  , units       :: Int
  } deriving (Generic, Show)

instance FromJSON CargoItem

data Cooldown = Cooldown
  { remainingSeconds :: Int
  , shipSymbol       :: T.Text
  , totalSeconds     :: Int
  } deriving (Generic, Show)

instance FromJSON Cooldown

data Fuel = Fuel
  { capacity :: Int
  , consumed :: FuelConsumed
  , current  :: Int
  } deriving (Generic, Show)

instance FromJSON Fuel

data FuelConsumed = FuelConsumed
  { amount    :: Int
  , timestamp :: UTCTime
  } deriving (Generic, Show)

instance FromJSON FuelConsumed

isDocked :: Ship -> Bool
isDocked ship = ship.nav.status == "DOCKED"

isOrbitting :: Ship -> Bool
isOrbitting = not . isDocked

data Nav = Nav
  { flightMode     :: T.Text
  , route          :: Route
  , status         :: T.Text
  , systemSymbol   :: T.Text
  , waypointSymbol :: T.Text
  } deriving (Generic, Show)

instance FromJSON Nav

data Route = Route
  { arrival       :: UTCTime
  , departureTime :: UTCTime
  , destination   :: RouteEndpoint
  , origin        :: RouteEndpoint
  } deriving (Generic, Show)

instance FromJSON Route

data RouteEndpoint = RouteEndpoint
  { type_        :: T.Text
  , symbol       :: T.Text
  , systemSymbol :: T.Text
  , x            :: Int
  , y            :: Int
  } deriving (Generic, Show)

instance FromJSON RouteEndpoint where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \x -> if x == "type_" then "type" else x }

data Ship = Ship
  { cargo    :: Cargo
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
  } deriving (Generic, Show)

instance FromJSON Ship
