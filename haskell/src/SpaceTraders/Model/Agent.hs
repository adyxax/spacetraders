{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Model.Agent
  ( Agent(..)
  ) where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

data Agent = Agent { accountId :: T.Text
                   , credits :: Integer
                   --, faction :: Faction
                   , headquarters :: T.Text
                   , startingFaction :: T.Text
                   , symbol :: T.Text
                   } deriving (Eq, FromJSON, Generic, Show, ToJSON)
