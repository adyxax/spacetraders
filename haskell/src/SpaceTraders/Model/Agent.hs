{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Model.Agent
  ( Agent(accountId, credits, headquarters)
  , agentSymbol
  ) where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

data Agent = Agent { accountId :: T.Text
                   , credits :: Integer
                   , headquarters :: T.Text
                   , startingFaction :: T.Text
                   , symbol :: T.Text
                   } deriving (FromJSON, Generic, Show, ToJSON)

agentSymbol :: Agent -> T.Text
agentSymbol = symbol
