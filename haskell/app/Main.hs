module Main (main) where

import           SpaceTraders
import           SpaceTraders.APIClient.Agent
import           SpaceTraders.APIClient.Contracts
import           SpaceTraders.APIClient.Ships
import           SpaceTraders.Automation.Init
import           SpaceTraders.Database.Ships

main :: IO ()
main = do
  env <- initST
  runSpaceTradersT main' env
  deinitST env
  where
    main' :: SpaceTradersT ()
    main' = do
      -- refresh our core objects
      (Right _) <- myAgent
      (Right _) <- myContracts
      (Right _) <- myShips
      -- Testing
      ships <- getShips
      let (cmdShip:_) = ships
      t <- refuel cmdShip
      liftIO . print $ case t of
        (Right r) -> "response: " ++ show r
        (Left e)  -> "error: " ++ show e
      return ()
