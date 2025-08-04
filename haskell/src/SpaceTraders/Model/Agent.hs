module SpaceTraders.Model.Agent
  ( Agent(..)
  ) where

import           Data.Aeson
import qualified Data.Text    as T
import           GHC.Generics

data Agent = Agent { accountId       :: T.Text
                   , credits         :: Integer
                   --, faction :: Faction
                   , headquarters    :: T.Text
                   , startingFaction :: T.Text
                   , symbol          :: T.Text
                   } deriving (Eq, FromJSON, Generic, Show, ToJSON)
