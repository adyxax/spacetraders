{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Ships
  ( addShip
  ) where

import Control.Monad.Reader
import Data.Aeson
import qualified Database.SQLite.Simple as S

import SpaceTraders
import SpaceTraders.Model.Ship

addShip :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Ship -> m ()
addShip ship = do
  env <- ask
  liftIO $ S.execute (getConn env) "INSERT INTO ships(data) VALUES (json(?));" (S.Only (encode ship))
