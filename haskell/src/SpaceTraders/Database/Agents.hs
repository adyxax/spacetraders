{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Agents
  ( addAgent
  ) where

import Control.Monad.Reader
import Data.Aeson
import qualified Database.SQLite.Simple as S

import SpaceTraders
import SpaceTraders.Model.Agent

addAgent :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Agent -> m ()
addAgent agent = do
  env <- ask
  liftIO $ S.execute (getConn env) "INSERT INTO agents(data) VALUES (json(?));" (S.Only (encode agent))
