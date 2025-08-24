module SpaceTraders.Model.Contract
  ( Contract(..)
  , Delivery(..)
  , Payment(..)
  , Terms(..)
  ) where

import           Data.Aeson
import qualified Data.Text    as T
import           Data.Time
import           GHC.Generics

data Contract = Contract
  { accepted         :: Bool
  , id               :: T.Text
  , type_            :: T.Text
  , expiration       :: UTCTime
  , deadlineToAccept :: UTCTime
  , factionSymbol    :: T.Text
  , fulfilled        :: Bool
  , terms            :: Terms
  } deriving (Generic, Show)

instance FromJSON Contract where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \x -> if x == "type_" then "type" else x }

instance ToJSON Contract where
  toJSON     = genericToJSON defaultOptions
    { fieldLabelModifier = \x -> if x == "type_" then "type" else x }

data Delivery = Delivery
  { destinationSymbol :: T.Text
  , tradeSymbol       :: T.Text
  , unitsFulfilled    :: Int
  , unitsRequired     :: Int
  } deriving (Generic, Show)

instance FromJSON Delivery
instance ToJSON Delivery

data Payment = Payment
  { onAccepted  :: Int
  , onFulfilled :: Int
  } deriving (Generic, Show)

instance FromJSON Payment
instance ToJSON Payment

data Terms = Terms
  { deadline :: UTCTime
  , deliver  :: [Delivery]
  , payment  :: Payment
  } deriving (Generic, Show)

instance FromJSON Terms
instance ToJSON Terms
