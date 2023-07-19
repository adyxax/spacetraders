{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Ships
  ( addShip
  , getShips
  , setShip
  , updateShip
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Maybe
import qualified Database.SQLite.Simple as S

import SpaceTraders
import SpaceTraders.Model.Ship
import SpaceTraders.Utils

addShip :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Ship -> m ()
addShip ship = do
  env <- ask
  liftIO $ S.execute (getConn env) "INSERT INTO ships(data) VALUES (json(?));" (S.Only $ encode ship)

getShips :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => m [Ship]
getShips = do
  env <- ask
  ret <- liftIO $ S.query_ (getConn env) "SELECT data FROM ships;"
  return . catMaybes $ map (decodeText . head) ret

setShip :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Ship -> m ()
setShip ship = do
  env <- ask
  count <- liftIO (S.query (getConn env) "SELECT count(id) FROM ships WHERE data->>'symbol' = ?;" (S.Only $ symbol ship) :: IO [[Int]])
  if count == [[0]] then addShip ship
                    else updateShip ship

updateShip :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Ship -> m ()
updateShip ship = do
  env <- ask
  liftIO $ S.execute (getConn env) "UPDATE ships SET data = json(?) WHERE data->>'symbol' = ?;" (encode ship, symbol ship)
