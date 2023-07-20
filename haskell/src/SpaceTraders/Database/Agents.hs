{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Agents
  ( addAgent
  , getAgent
  , setAgent
  ) where

import Control.Monad.Reader
import Data.Aeson
import qualified Database.SQLite.Simple as S

import SpaceTraders
import SpaceTraders.Model.Agent
import SpaceTraders.Utils

addAgent :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Agent -> m ()
addAgent agent = execute "INSERT INTO agents(data) VALUES (json(?));" (S.Only (encode agent))

getAgent :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => m Agent
getAgent = one_ "SELECT data FROM agents";

setAgent :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Agent -> m ()
setAgent agent = execute "UPDATE agents SET data = json(?);" (S.Only (encode agent))
