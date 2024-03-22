module Main (main) where

import           SpaceTraders
import           SpaceTraders.APIClient.Agent
import           SpaceTraders.APIClient.Contracts
import           SpaceTraders.APIClient.Ships
import           SpaceTraders.Automation.Init

main :: IO ()
main = do
  env <- initST
  runSpaceTradersT main' env
  deinitST env
  where
    main' :: SpaceTradersT ()
    main' = do
      -- refresh our core objects
      _ <- myAgent
      _ <- myContracts
      (Right ships) <- myShips -- work around to fetch the initial probe
      let cmdShip = head ships
      (Right t) <- orbit cmdShip
      liftIO $ print t
      return ()
