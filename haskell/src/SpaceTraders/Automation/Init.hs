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
import SpaceTraders.APIClient.Contracts
import SpaceTraders.APIClient.Errors
import SpaceTraders.APIClient.Ships
import SpaceTraders.Database
import SpaceTraders.Database.Agents
import SpaceTraders.Database.Contracts
import SpaceTraders.Database.Tokens

deinitST :: Env -> IO ()
deinitST env = do
  close $ getConn env

initST :: IO Env
initST = do
  conn <- open
  t <- runReaderT getToken conn `catch` handleNoToken conn
  env <- newEnv conn (tokenReq t)
  ma <- runReaderT getAgent conn -- We compare the agent state in the database
  ma' <- runSpaceTradersT myAgent env -- with the one on the servers
  case ma' of
    Left (APIResetHappened _) -> wipe conn
    Left e -> throwIO e
    Right ma'' -> do
      when (ma /= ma'') $ do
        _ <- runReaderT myContracts env -- refresh contracts
        _ <- runReaderT myShips env -- refresh ships
        runReaderT (setAgent ma'') conn -- store the fresh agent state
      return $ env
  where
    handleNoToken :: S.Connection -> SomeException -> IO T.Text
    handleNoToken conn _ = newEnv conn defaultReq >>= runReaderT registerST

registerST :: SpaceTradersT (T.Text)
registerST = do
  r <- register "ADYXAX" "COSMIC"
  case r of
    Right r' -> do
      addAgent $ agent r'
      addContract $ contract r'
      _ <- myShips -- in order to fetch the starting probe that is not advertised in the register message
      let t = token r'
      addToken t
      return t
    Left e' -> throw e'

wipe :: S.Connection -> IO Env
wipe c = do
      close c
      removeFile "spacetraders.db"
      conn' <- open
      t <- newEnv conn' defaultReq >>= runReaderT registerST
      newEnv conn' (tokenReq t)
