module SpaceTraders.APIClient.Contracts
  ( myContracts
  ) where

import           Network.HTTP.Simple

import           SpaceTraders
import           SpaceTraders.APIClient.Client
import           SpaceTraders.APIClient.Pagination
import           SpaceTraders.Model.Contract       (Contract)

myContracts :: SpaceTradersT (APIResponse [Contract])
myContracts = do
  listContracts' [] Pagination{limit=20, page=1, total=0}
  where
    listContracts' :: [Contract] -> Pagination -> SpaceTradersT (APIResponse [Contract])
    listContracts' acc p = do
      resp <- sendPaginated (Just p) $ setRequestPath "/v2/my/contracts" :: SpaceTradersT (APIPaginatedResponse [Contract])
      case resp of
        Left e -> return $ Left e
        Right (APIMessage r (Just p')) -> do
          if limit p' * page p' < total p' then listContracts' (acc ++ r) (nextPage p')
                                           else pure $ Right acc
        _ -> undefined
