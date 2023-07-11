{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SpaceTraders.Database
  ( close
  , open
  ) where

import Control.Exception
import qualified Database.SQLite.Simple as S
import Text.RawString.QQ

migrations :: [S.Query]
migrations = [
  [r|CREATE TABLE schema_version (
       version INTEGER NOT NULL
     );|],
  [r|CREATE TABLE tokens (
       id INTEGER PRIMARY KEY,
       data TEXT NOT NULL
     );|],
  [r|CREATE TABLE agents (
       id INTEGER PRIMARY KEY,
       data TEXT NOT NULL
     );|],
  [r|CREATE TABLE contracts (
       id INTEGER PRIMARY KEY,
       data TEXT NOT NULL
     );|],
  [r|CREATE TABLE ships (
       id INTEGER PRIMARY KEY,
       data TEXT NOT NULL
     );|],
  [r|CREATE UNIQUE INDEX ships_data_symbol ON ships (json_extract(data, '$.symbol'));|],
  [r|CREATE TABLE systems (
       id INTEGER PRIMARY KEY,
       data TEXT NOT NULL
     );|],
  [r|CREATE UNIQUE INDEX systems_data_symbol ON systems (json_extract(data, '$.symbol'));|]]

close :: S.Connection -> IO ()
close conn = S.close conn

open :: IO S.Connection
open = do
  conn <- S.open "spacetraders.db"
  S.execute_ conn "PRAGMA foreign_keys = ON;"
  S.execute_ conn "PRAGMA journal_mode = WAL;"
  S.withTransaction conn $ do
    version <- getSchemaVersion conn `catch` defaultVersion
    mapM_ (S.execute_ conn) $ drop version migrations
    S.execute_ conn "DELETE FROM schema_version;"
    S.execute conn "INSERT INTO schema_version (version) VALUES (?);" (S.Only $ length migrations)
  return conn

getSchemaVersion :: S.Connection -> IO Int
getSchemaVersion conn = do
  [[v]] <- S.query_ conn "SELECT version FROM schema_version;"
  return v

defaultVersion :: SomeException -> IO Int
defaultVersion _ = return 0
