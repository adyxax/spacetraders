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

data NavMessage = NavMessage
  { nav :: Nav
  } deriving (Generic, Show)

instance FromJSON NavMessage

dock :: Ship -> SpaceTradersT (ApiResponse Ship)
dock ship | isDocked ship = pure $ Right ship
          | otherwise = do
              resp <- send $ setRequestMethod "POST"
                           . setRequestPath (T.encodeUtf8 $ mconcat ["/v2/my/ships/", ship.symbol, "/dock"])
                           :: SpaceTradersT (ApiResponse NavMessage)
              pure $ case resp of
                Left e                 -> Left e
                Right (NavMessage nav) -> Right $ ship { SpaceTraders.Model.Ship.nav = nav }

myShips :: SpaceTradersT (ApiResponse [Ship])
myShips = send $ setRequestPath "/v2/my/ships"

orbit :: Ship -> SpaceTradersT (ApiResponse Ship)
orbit ship | isOrbitting ship = pure $ Right ship
           | otherwise = do
               resp <- send $ setRequestMethod "POST"
                            . setRequestPath (T.encodeUtf8 $ mconcat ["/v2/my/ships/", ship.symbol, "/orbit"])
                            :: SpaceTradersT (ApiResponse NavMessage)
               pure $ case resp of
                 Left e                 -> Left e
                 Right (NavMessage nav) -> Right $ ship { SpaceTraders.Model.Ship.nav = nav }
