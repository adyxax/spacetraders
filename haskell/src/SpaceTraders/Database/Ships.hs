{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Ships
  ( addShip
  ) where

import Data.Aeson
import qualified Database.SQLite.Simple as S

import SpaceTraders.Model.Ship

addShip :: S.Connection -> Ship -> IO ()
addShip conn ship = S.execute conn "INSERT INTO ships(data) VALUES (json(?));" (S.Only (encode ship))
