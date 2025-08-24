module SpaceTraders.Model.Agent
  ( Agent(..)
  , nullAgent
  ) where

import           Data.Aeson
import           Data.Text
import           GHC.Generics

data Agent = Agent { accountId    :: Text
                   , credits      :: Integer
                   --, faction :: Faction
                   , headquarters :: Text
                   --, startingFaction :: Text
                   , symbol       :: Text
                   } deriving (Generic, Show)

instance FromJSON Agent
instance ToJSON Agent

nullAgent :: Agent
nullAgent = Agent { accountId = ""
                  , credits = 0
                  , headquarters = ""
                  , symbol = "" }
