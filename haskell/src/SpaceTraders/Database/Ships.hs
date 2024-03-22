{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Ships
  ( getShips
  , setShip
  ) where

import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Data.Aeson
import qualified Database.SQLite.Simple    as S

import           SpaceTraders
import           SpaceTraders.Model.Ship
import           SpaceTraders.Utils

getShips :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => m [Ship]
getShips = query_ "SELECT data FROM ships;"

setShip :: (HasDatabaseConn env, MonadError e m, MonadIO m, MonadReader env m) => Ship -> m ()
setShip ship = updateShip `catchError` addShip
  where
    addShip _ = execute "INSERT INTO ships(data) VALUES (json(?));" (S.Only $ encode ship)
    updateShip = execute "UPDATE ships SET data = json(?) WHERE data->>'symbol' = ?;" (encode ship, symbol ship)
