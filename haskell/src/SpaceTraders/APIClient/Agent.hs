{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.APIClient.Agent
  ( RegisterMessage(..)
  , myAgent
  , register
  ) where

import Control.Monad.Reader
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import Network.HTTP.Simple

import SpaceTraders
import SpaceTraders.APIClient.Client
import SpaceTraders.Model.Agent(Agent)
import SpaceTraders.Model.Ship(Ship)
import SpaceTraders.Model.Contract

myAgent :: SpaceTradersT (APIResponse Agent)
myAgent = send $ setRequestPath "/v2/my/agent"

data RegisterRequest = RegisterRequest { faction :: T.Text
                                       , symbol :: T.Text
                                       } deriving (ToJSON, Generic, Show)
data RegisterMessage = RegisterMessage { agent :: Agent
                                       , contract :: Contract
                                       , ship :: Ship
                                       , token :: T.Text
                                       } deriving (FromJSON, Generic, Show)

register :: (HasRequest env, MonadIO m, MonadReader env m) => T.Text -> T.Text -> m (APIResponse RegisterMessage)
register s f = send $ setRequestPath "/v2/register"
                    . setRequestMethod "POST"
                    . setRequestBodyJSON RegisterRequest{symbol = s, faction = f}
