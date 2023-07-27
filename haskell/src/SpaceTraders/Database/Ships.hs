{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Ships
  ( addShip
  , getShips
  , setShip
  , updateShip
  ) where

import Control.Monad.Reader
import Data.Aeson
import Data.Time
import Data.Time.Format.ISO8601
import qualified Database.SQLite.Simple as S

import SpaceTraders
import SpaceTraders.Model.Ship
import SpaceTraders.Utils

addShip :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Ship -> m ()
addShip ship = do
  t <- liftIO getCurrentTime
  execute "INSERT INTO ships(data, available) VALUES (json(?), ?);" (encode ship, iso8601Show t)

getShips :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => m [Ship]
getShips = query_ "SELECT data FROM ships;"

setShip :: (HasDatabaseConn env, MonadFail m, MonadIO m, MonadReader env m) => Ship -> m ()
setShip ship = do
  c <- count "SELECT count(id) FROM ships WHERE data->>'symbol' = ?;" (S.Only $ symbol ship)
  if c == 0 then addShip ship
            else updateShip ship Nothing

updateShip :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Ship -> Maybe UTCTime -> m ()
updateShip ship (Just time) = execute "UPDATE ships SET data = json(?), available = ? WHERE data->>'symbol' = ?;" (encode ship, iso8601Show time, symbol ship)
updateShip ship Nothing = execute "UPDATE ships SET data = json(?) WHERE data->>'symbol' = ?;" (encode ship, symbol ship)
