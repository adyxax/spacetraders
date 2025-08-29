module SpaceTraders.ApiClient.Ships
  ( dock
  , myShips
  , orbit
  ) where

import           Data.Aeson.Types
import qualified Data.List                     as L
import qualified Data.Text.Encoding            as T
import           GHC.Generics
import           Network.HTTP.Simple

import           SpaceTraders
import           SpaceTraders.ApiClient.Client
import           SpaceTraders.Model.Ship

dock :: Ship -> SpaceTradersT Ship
dock ship | isDocked ship = pure ship
          | otherwise = do
              resp <- send $ setRequestMethod "POST"
                           . setRequestPath (T.encodeUtf8 $ mconcat ["/v2/my/ships/", ship.symbol, "/dock"])
                           :: SpaceTradersT DockResponse
              pure $ ship { SpaceTraders.Model.Ship.nav = resp.nav }

data DockResponse = DockResponse
  { nav :: Nav
  } deriving (Generic, Show)

instance FromJSON DockResponse

myShips :: SpaceTradersT [Ship]
myShips = send $ setRequestPath "/v2/my/ships"

orbit :: Ship -> SpaceTradersT Ship
orbit ship | isOrbitting ship = pure ship
           | otherwise = do
               resp <- send $ setRequestMethod "POST"
                            . setRequestPath (T.encodeUtf8 $ mconcat ["/v2/my/ships/", ship.symbol, "/orbit"])
                            :: SpaceTradersT DockResponse
               pure $ ship { SpaceTraders.Model.Ship.nav = resp.nav }
