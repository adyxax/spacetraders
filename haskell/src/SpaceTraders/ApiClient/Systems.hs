module SpaceTraders.ApiClient.Systems
  ( getSystem
  , listWaypoints
  ) where

import           Control.Exception
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import           Network.HTTP.Simple

import           SpaceTraders
import           SpaceTraders.ApiClient.Client
import           SpaceTraders.ApiClient.Pagination
import           SpaceTraders.Model.System

getSystem :: T.Text -> SpaceTradersT System
getSystem symbol = send $ setRequestPath (T.encodeUtf8 $ mconcat ["/v2/systems/", symbol])

listWaypoints :: System -> SpaceTradersT [Waypoint]
listWaypoints system = sendPaginated firstPage $ setRequestPath (T.encodeUtf8 $ mconcat ["/v2/systems/", system.symbol, "/waypoints"])
