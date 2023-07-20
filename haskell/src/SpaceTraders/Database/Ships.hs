{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Ships
  ( addShip
  , getShips
  , setShip
  , updateShip
  ) where

import Control.Monad.Reader
import Data.Aeson
import qualified Database.SQLite.Simple as S

import SpaceTraders
import SpaceTraders.Model.Ship
import SpaceTraders.Utils

addShip :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Ship -> m ()
addShip ship = execute "INSERT INTO ships(data) VALUES (json(?));" (S.Only $ encode ship)

getShips :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => m [Ship]
getShips = query_ "SELECT data FROM ships;"

setShip :: (HasDatabaseConn env, MonadFail m, MonadIO m, MonadReader env m) => Ship -> m ()
setShip ship = do
  c <- count "SELECT count(id) FROM ships WHERE data->>'symbol' = ?;" (S.Only $ symbol ship)
  if c == 0 then addShip ship
            else updateShip ship

updateShip :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Ship -> m ()
updateShip ship = execute "UPDATE ships SET data = json(?) WHERE data->>'symbol' = ?;" (encode ship, symbol ship)
