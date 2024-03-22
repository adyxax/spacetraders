{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module SpaceTraders.Database
  ( close
  , open
  ) where

import           Control.Exception
import qualified Data.ByteString        as B
import           Data.FileEmbed
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Database.SQLite.Simple as S

migrations :: [B.ByteString]
migrations = [ $(embedFile "src/SpaceTraders/Database/000_init.sql") ]

close :: S.Connection -> IO ()
close = S.close

open :: IO S.Connection
open = do
  conn <- S.open "spacetraders.db"
  S.execute_ conn "PRAGMA foreign_keys = ON;"
  S.execute_ conn "PRAGMA journal_mode = WAL;"
  S.withTransaction conn $ do
    version <- getSchemaVersion conn `catch` defaultVersion
    mapM_ (S.execute_ conn) $ S.Query <$> concatMap (filter (/= "\n") . T.splitOn ";" . T.decodeUtf8) (drop version migrations)
    S.execute_ conn "DELETE FROM schema_version;"
    S.execute conn "INSERT INTO schema_version (version) VALUES (?);" (S.Only $ length migrations)
  return conn

getSchemaVersion :: S.Connection -> IO Int
getSchemaVersion conn = do
  [[v]] <- S.query_ conn "SELECT version FROM schema_version;"
  return v

defaultVersion :: SomeException -> IO Int
defaultVersion _ = return 0
