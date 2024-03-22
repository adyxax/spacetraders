{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Agents
  ( getAgent
  , setAgent
  ) where

import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Data.Aeson
import qualified Database.SQLite.Simple    as S

import           SpaceTraders
import           SpaceTraders.Model.Agent
import           SpaceTraders.Utils

getAgent :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => m Agent
getAgent = one_ "SELECT data FROM agents"; -- we only support one agent at a time

setAgent :: (HasDatabaseConn env, MonadError e m, MonadIO m, MonadReader env m) => Agent -> m ()
setAgent agent = updateAgent `catchError` addAgent
  where
    addAgent _ = execute "INSERT INTO agents(data) VALUES (json(?));" (S.Only $ encode agent)
    updateAgent = execute "UPDATE agents SET data = json(?);" (S.Only (encode agent))
