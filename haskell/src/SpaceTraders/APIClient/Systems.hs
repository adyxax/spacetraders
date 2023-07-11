{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.APIClient.Systems
  ( listSystems
  ) where

import Control.Exception
import qualified Data.Text as T
import qualified Database.SQLite.Simple as S
import Network.HTTP.Simple

import SpaceTraders.APIClient.Client
import SpaceTraders.APIClient.Pagination
import SpaceTraders.Database.Systems
import SpaceTraders.Model.System(System)

listSystems :: T.Text -> S.Connection -> IO (APIResponse [System])
listSystems t conn = do
  s <- getSystems conn
  listSystems' Pagination{limit=20, page=((length s) `div` 20) + 1, total=0}
  where
    listSystems' :: Pagination -> IO (APIResponse [System])
    listSystems' p = do
      resp <- sendPaginated $ setRequestPath "/v2/systems"
                            $ paginatedReq t (Just p)
      case resp of
        Left e -> throwIO e
        Right (APIMessage [] _) -> Right <$> getSystems conn
        Right (APIMessage r (Just p')) -> do
          addSystems conn r
          listSystems' (nextPage p')
        _ -> undefined
