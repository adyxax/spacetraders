{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Automation.Init
  ( deinitST
  , initST
  ) where

import Control.Exception
import qualified Database.SQLite.Simple as S
import qualified Data.Text as T
import System.Directory

import SpaceTraders
import qualified SpaceTraders.APIClient.Agent as STAA
import SpaceTraders.APIClient.Errors
import SpaceTraders.Database
import SpaceTraders.Database.Agents
import SpaceTraders.Database.Contracts
import SpaceTraders.Database.Ships
import SpaceTraders.Database.Tokens

deinitST :: Config -> IO ()
deinitST config = do
  close $ conn config

initST :: IO Config
initST = do
  c <- open
  t <- getToken c `catch` handleNoToken c
  ma <- runSpaceTradersT STAA.myAgent (Config c t)
  case ma of
    Left (APIResetHappened _) -> wipe c
    Left e -> throwIO e
    _ -> return $ Config c t
  where
    handleNoToken :: S.Connection -> SomeException -> IO T.Text
    handleNoToken c _ = register c

register :: S.Connection -> IO (T.Text)
register c = do
  r <- STAA.register "ADYXAX" "COSMIC"
  case r of
    Right r' -> do
      setAgent c $ STAA.agent r'
      addContract c $ STAA.contract r'
      addShip c $ STAA.ship r'
      let t = STAA.token r'
      setToken c $ t
      return t
    Left e' -> throwIO e'

wipe :: S.Connection -> IO Config
wipe c = do
      close c
      removeFile "spacetraders.db"
      conn' <- open
      t <- register conn'
      return $ Config conn' t
