{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.APIClient.Ships
  ( myShips
  ) where

import Network.HTTP.Simple

import SpaceTraders
import SpaceTraders.APIClient.Client
import SpaceTraders.APIClient.Pagination
import SpaceTraders.Database.Ships
import SpaceTraders.Model.Ship(Ship)

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
          if (limit p' * page p' < total p') then listShips' (nextPage p')
                                             else Right <$> getShips
        _ -> undefined
