{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Systems
  ( addSystems
  , getSystems
  ) where

import Control.Monad.Reader
import Data.Aeson
import Data.Maybe
import qualified Database.SQLite.Simple as S

import SpaceTraders
import SpaceTraders.Model.System
import SpaceTraders.Utils

addSystems :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => [System] -> m ()
addSystems systems = do
  env <- ask
  let conn = getConn env
  liftIO $ S.withTransaction conn $ S.executeMany conn "INSERT INTO systems(data) VALUES (json(?));" $ S.Only <$> map encode systems

getSystems :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => m [System]
getSystems = do
  env <- ask
  ret <- liftIO $ S.query_ (getConn env) "SELECT data FROM systems;"
  return . catMaybes $ map (decodeText . head) ret
