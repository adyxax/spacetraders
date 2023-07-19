{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import System.Environment
import System.Posix.Process

import SpaceTraders
import SpaceTraders.Automation.Init
import SpaceTraders.APIClient.Errors
import SpaceTraders.APIClient.Systems
import SpaceTraders.Database.Agents
import SpaceTraders.Database.Contracts
import SpaceTraders.Database.Ships

main :: IO ()
main = do
  env <- initST
  runSpaceTradersT getAgent env >>= print
  s <- runSpaceTradersT initSystems env
  case s of
    Left (APIResetHappened _) -> do
      p <- getExecutablePath
      a <- getArgs
      e <- getEnvironment
      executeFile p False a (Just e)
    Left e -> throwIO e
    Right s' -> print $ length s'
  runSpaceTradersT getContracts env >>= print
  runSpaceTradersT getShips env >>= print
  deinitST env
