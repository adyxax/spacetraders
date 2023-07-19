{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Agents
  ( addAgent
  , getAgent
  , setAgent
  ) where

import Control.Monad.Reader
import Data.Aeson
import Data.Maybe
import qualified Database.SQLite.Simple as S

import SpaceTraders
import SpaceTraders.Model.Agent
import SpaceTraders.Utils

addAgent :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Agent -> m ()
addAgent agent = do
  env <- ask
  liftIO $ S.execute (getConn env) "INSERT INTO agents(data) VALUES (json(?));" (S.Only (encode agent))

getAgent :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => m Agent
getAgent = do
  env <- ask
  ret <- liftIO $ S.query_ (getConn env) "SELECT data FROM agents;"
  return . head . catMaybes $ map (decodeText . head) ret

setAgent :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Agent -> m ()
setAgent agent = do
  env <- ask
  liftIO $ S.execute (getConn env) "UPDATE agents SET data = json(?);" (S.Only (encode agent))
