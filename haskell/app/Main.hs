{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import System.Environment
import System.Posix.Process

import SpaceTraders
import SpaceTraders.Automation.Init
import SpaceTraders.APIClient.Agent(myAgent)
import SpaceTraders.APIClient.Client
import SpaceTraders.APIClient.Ships
import SpaceTraders.APIClient.Systems

main :: IO ()
main = do
  env <- initST
  ma <- runSpaceTradersT myAgent env
  case ma of
   Left (APIResetHappened _) -> do
     p <- getExecutablePath
     a <- getArgs
     e <- getEnvironment
     executeFile p False a (Just e)
   Left e -> throwIO e
   Right ma' -> print ma'
  s <- runSpaceTradersT listSystems env
  case s of
    Left e -> throwIO e
    Right s' -> print $ length s'
  ships <- runSpaceTradersT listShips env
  case ships of
    Left e -> throwIO e
    Right s' -> print $ s'
  deinitST env
