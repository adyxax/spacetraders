{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import qualified Database.SQLite.Simple as S
import qualified Data.Text as T

import SpaceTraders.APIClient.Agent
import SpaceTraders.APIClient.Systems
import SpaceTraders.Database
import SpaceTraders.Database.Agents
import SpaceTraders.Database.Contracts
import SpaceTraders.Database.Ships
import SpaceTraders.Database.Tokens

main :: IO ()
main = do
  conn <- open
  t <- getToken conn `catch` registerNow conn
  ma <- myAgent t
  print ma
  s <- listSystems t conn
  print s
  close conn
  where
    registerNow :: S.Connection -> SomeException -> IO (T.Text)
    registerNow conn _ = do
      r <- register "ADYXAX" "COSMIC"
      case r of
        Right r' -> do
          setAgent conn $ agent r'
          addContract conn $ contract r'
          addShip conn $ ship r'
          let t = token r'
          setToken conn $ t
          return t
        Left e' -> throwIO e'
