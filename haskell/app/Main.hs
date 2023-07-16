{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception

import SpaceTraders
import SpaceTraders.Automation.Init
import SpaceTraders.APIClient.Agent(myAgent)
import SpaceTraders.APIClient.Systems

main :: IO ()
main = do
  config <- initST
  ma <- runSpaceTradersT myAgent config
  print ma
  s <- listSystems (token config) (conn config)
  case s of
    Left e -> throwIO e
    Right s' -> print $ length s'
  deinitST config
