{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Automation.Init
  ( deinitST
  , initST
  ) where

import Control.Exception
import Control.Monad.Reader
import qualified Database.SQLite.Simple as S
import qualified Data.Text as T
import System.Directory

import SpaceTraders
import SpaceTraders.APIClient.Agent
import SpaceTraders.APIClient.Client
import SpaceTraders.Database
import SpaceTraders.Database.Agents
import SpaceTraders.Database.Contracts
import SpaceTraders.Database.Ships
import SpaceTraders.Database.Tokens

deinitST :: Env -> IO ()
deinitST env = do
  close $ getConn env

initST :: IO Env
initST = do
  conn <- open
  t <- runReaderT getToken conn `catch` handleNoToken conn
  let env = Env conn (tokenReq t)
  ma <- runSpaceTradersT myAgent env
  case ma of
    Left (APIResetHappened _) -> wipe conn
    Left e -> throwIO e
    _ -> return $ env
  where
    handleNoToken :: S.Connection -> SomeException -> IO T.Text
    handleNoToken conn _ = runReaderT registerST (Env conn defaultReq)

registerST :: SpaceTradersT (T.Text)
registerST = do
  r <- register "ADYXAX" "COSMIC"
  case r of
    Right r' -> do
      addAgent $ agent r'
      addContract $ contract r'
      addShip $ ship r'
      let t = token r'
      addToken t
      return t
    Left e' -> throw e'

wipe :: S.Connection -> IO Env
wipe c = do
      close c
      removeFile "spacetraders.db"
      conn' <- open
      t <- runReaderT registerST (Env conn' defaultReq)
      return $ Env conn' (tokenReq t)
