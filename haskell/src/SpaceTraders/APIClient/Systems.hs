module SpaceTraders.APIClient.Systems
  ( initSystems
  ) where

import           Control.Exception
import           Network.HTTP.Simple

import           SpaceTraders
import           SpaceTraders.APIClient.Client
import           SpaceTraders.APIClient.Pagination
import           SpaceTraders.Database.Systems
import           SpaceTraders.Model.System         (System)

initSystems :: SpaceTradersT (APIResponse [System])
initSystems = do
  s <- getSystems
  listSystems' Pagination{limit=20, page=(length s `div` 20) + 1, total=0}
  where
    listSystems' :: Pagination -> SpaceTradersT (APIResponse [System])
    listSystems' p = do
      resp <- sendPaginated (Just p) $ setRequestPath "/v2/systems"
      case resp of
        Left e -> throw e
        Right (APIMessage r (Just p')) -> do
          addSystems r
          if limit p' * page p' < total p' then listSystems' (nextPage p')
                                           else Right <$> getSystems
        _ -> undefined
