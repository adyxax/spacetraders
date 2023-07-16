{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.APIClient.Agent
  ( RegisterMessage(..)
  , myAgent
  , register
  ) where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
import Network.HTTP.Simple

import qualified SpaceTraders as ST
import SpaceTraders.APIClient.Client
import SpaceTraders.Model.Agent(Agent)
import SpaceTraders.Model.Ship(Ship)
import SpaceTraders.Model.Contract

myAgent :: ST.SpaceTradersT (APIResponse Agent)
myAgent = do
  c <- ST.ask
  ST.liftIO $ send $ setRequestPath "/v2/my/agent"
            $ tokenReq (ST.token c)

data RegisterRequest = RegisterRequest { faction :: T.Text
                                       , symbol :: T.Text
                                       } deriving (ToJSON, Generic, Show)
data RegisterMessage = RegisterMessage { agent :: Agent
                                       , contract :: Contract
                                       , ship :: Ship
                                       , token :: T.Text
                                       } deriving (FromJSON, Generic, Show)

register :: T.Text -> T.Text -> IO (APIResponse RegisterMessage)
register s f = send $ setRequestPath "/v2/register"
                    $ setRequestMethod "POST"
                    $ setRequestBodyJSON RegisterRequest{symbol = s, faction = f}
                    $ defaultReq
