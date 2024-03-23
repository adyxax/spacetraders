{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.APIClient.Ships
  ( dock
  , myShips
  , orbit
  , refuel
  ) where

import           Data.Aeson.Types
import qualified Data.Text.Encoding                as T
import           GHC.Generics
import           Network.HTTP.Simple

import           SpaceTraders
import           SpaceTraders.APIClient.Client
import           SpaceTraders.APIClient.Pagination
import           SpaceTraders.Database.Agents
import           SpaceTraders.Database.Ships
import qualified SpaceTraders.Model.Agent
import           SpaceTraders.Model.Fuel
import           SpaceTraders.Model.Nav
import           SpaceTraders.Model.Ship

newtype NavMessage = NavMessage { nav :: Nav } deriving (FromJSON, Generic, Show)

dock :: Ship -> SpaceTradersT (APIResponse Ship)
dock ship = if isDocked ship then pure (Right ship) else dock'
  where
    dock' = do
      resp <- send $ setRequestPath (T.encodeUtf8 $ mconcat ["/v2/my/ships/", symbol ship, "/dock"])
                   . setRequestMethod "POST" :: SpaceTradersT (APIResponse NavMessage)
      case resp of
        Left e -> return $ Left e
        Right (NavMessage n) -> do
          let s = ship{SpaceTraders.Model.Ship.nav=n}
          setShip s
          return $ Right s

myShips :: SpaceTradersT (APIResponse [Ship])
myShips = do
  listShips' Pagination{limit=20, page=1, total=0}
  where
    listShips' :: Pagination -> SpaceTradersT (APIResponse [Ship])
    listShips' p = do
      resp <- sendPaginated (Just p) $ setRequestPath "/v2/my/ships" :: SpaceTradersT (APIPaginatedResponse [Ship])
      case resp of
        Left e -> return $ Left e
        Right (APIMessage r (Just p')) -> do
          mapM_ setShip r
          if limit p' * page p' < total p' then listShips' (nextPage p')
                                           else Right <$> getShips
        _ -> undefined

orbit :: Ship -> SpaceTradersT (APIResponse Ship)
orbit ship = if isInOrbit ship then pure (Right ship) else orbit'
  where
    orbit' = do
      resp <- send $ setRequestPath (T.encodeUtf8 $ mconcat ["/v2/my/ships/", symbol ship, "/orbit"])
                   . setRequestMethod "POST" :: SpaceTradersT (APIResponse NavMessage)
      case resp of
        Left e -> return $ Left e
        Right (NavMessage n) -> do
          let s = ship{SpaceTraders.Model.Ship.nav=n}
          setShip s
          return $ Right s

data RefuelMessage = RefuelMessage { agent :: SpaceTraders.Model.Agent.Agent
                                   , fuel  :: Fuel
                                   } deriving (FromJSON, Generic, Show)

refuel :: Ship -> SpaceTradersT (APIResponse Ship)
refuel ship = if overNinetyPercentFuel ship then pure (Right ship) else refuel'
  where
    refuel' = do
      resp <- dock ship
      case resp of
        (Left e)      -> return $ Left e
        (Right ship') -> refuel'' ship'
    refuel'' ship' = do
      -- TODO check if we are at a marketplace, that sells fuel
      resp' <- send $ setRequestPath (T.encodeUtf8 $ mconcat ["/v2/my/ships/", symbol ship, "/refuel"])
                    . setRequestMethod "POST" :: SpaceTradersT (APIResponse RefuelMessage)
      case resp' of
        Left e -> return $ Left e
        Right (RefuelMessage a f) -> do
          setAgent a
          let s = ship'{SpaceTraders.Model.Ship.fuel=f}
          setShip s
          return $ Right s
