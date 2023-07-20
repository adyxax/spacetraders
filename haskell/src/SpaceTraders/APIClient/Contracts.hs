{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.APIClient.Contracts
  ( myContracts
  ) where

import Network.HTTP.Simple

import SpaceTraders
import SpaceTraders.APIClient.Client
import SpaceTraders.APIClient.Pagination
import SpaceTraders.Database.Contracts
import SpaceTraders.Model.Contract(Contract)

myContracts :: SpaceTradersT (APIResponse [Contract])
myContracts = do
  listContracts' Pagination{limit=20, page=1, total=0}
  where
    listContracts' :: Pagination -> SpaceTradersT (APIResponse [Contract])
    listContracts' p = do
      resp <- sendPaginated (Just p) $ setRequestPath "/v2/my/contracts" :: SpaceTradersT (APIPaginatedResponse [Contract])
      case resp of
        Left e -> return $ Left e
        Right (APIMessage r (Just p')) -> do
          mapM_ setContract r
          if (limit p' * page p' < total p') then listContracts' (nextPage p')
                                             else Right <$> getContracts
        _ -> undefined
