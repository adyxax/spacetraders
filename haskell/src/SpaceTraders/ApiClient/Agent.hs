module SpaceTraders.ApiClient.Agent
  ( myAgent
  , RegisterData(..)
  , register
  ) where

import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Text                     as T
import           GHC.Generics
import           Network.HTTP.Simple

import           SpaceTraders
import           SpaceTraders.ApiClient.Client
import           SpaceTraders.Model.Agent

myAgent :: SpaceTradersT Agent
myAgent = send $ setRequestPath "/v2/my/agent"

data RegisterData = RegisterData { agent :: Agent
                                 --, contract :: Contract
                                 --, ship     :: Ship
                                 , token :: T.Text
                                 } deriving (Generic, Show)

instance FromJSON RegisterData

data RegisterRequest = RegisterRequest { faction :: T.Text
                                       , symbol  :: T.Text
                                       } deriving (Generic, Show)

instance ToJSON RegisterRequest

register :: T.Text -> T.Text -> SpaceTradersT RegisterData
register faction symbol  = send $ setRequestMethod "POST"
                                . setRequestPath "/v2/register"
                                . setRequestBodyJSON (RegisterRequest faction symbol)
