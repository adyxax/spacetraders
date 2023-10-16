{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import SpaceTraders
import SpaceTraders.Automation.Init
import SpaceTraders.APIClient.Ships
import SpaceTraders.APIClient.Systems

main :: IO ()
main = do
  env <- initST
  runSpaceTradersT main' env
  deinitST env
  where
    main' :: SpaceTradersT ()
    main' = do
      _ <- initSystems
      (Right ships) <- myShips -- work around to fetch the initial probe
      _ <- orbit (head ships)
      return ()
