{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.APIClient.Ships
  ( listShips
  ) where

import Control.Exception
--import qualified Data.Text as T
--import qualified Database.SQLite.Simple as S
import Network.HTTP.Simple

import SpaceTraders
import SpaceTraders.APIClient.Client
import SpaceTraders.APIClient.Pagination
--import SpaceTraders.Database.Ships
import SpaceTraders.Model.Ship(Ship)
import Debug.Trace

listShips :: SpaceTradersT (APIResponse [Ship])
listShips = do
  listShips' Pagination{limit=20, page=1, total=0}
  where
    listShips' :: Pagination -> SpaceTradersT (APIResponse [Ship])
    listShips' p = do
      resp <- sendPaginated (Just p) $ setRequestPath "/v2/my/ships"
      case resp of
        Left e -> throw e
        Right (APIMessage r (Just p')) -> do
          liftIO $ traceIO $ show p'
          --if (length r == 0 || ((page p') * (limit p')
          --addShips conn r
          --listShips' (nextPage p')
          return $ Right  r
        _ -> undefined
