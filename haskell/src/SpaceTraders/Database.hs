{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module SpaceTraders.Database
  ( close
  , count
  , decodeText
  , execute
  , one_
  , open
  , query
  , query_
  , wipe
  ) where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as B
import           Data.FileEmbed
import qualified Data.List               as L
import           Data.Maybe
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Database.SQLite.Simple  as S
import           SpaceTraders

close :: S.Connection -> IO ()
close = S.close

count :: S.ToRow t => S.Query -> t -> SpaceTradersT Int
count q t = do
  env <- ask
  [[ret]] <- liftIO (S.query env.dbConn q t :: IO [[Int]])
  return ret

decodeText :: FromJSON a => T.Text -> Maybe a
decodeText = decode . B.toLazyByteString . T.encodeUtf8Builder

defaultVersion :: SomeException -> IO Int
defaultVersion _ = pure 0

execute :: S.ToRow t => S.Query -> t -> SpaceTradersT ()
execute q t = do
  env <- ask
  liftIO $ S.execute env.dbConn q t

getSchemaVersion :: S.Connection -> IO Int
getSchemaVersion conn = do
  [S.Only v] <- S.query_ conn "SELECT version FROM schema_version;"
  pure v

migrations :: [B.ByteString]
migrations = [ $(embedFile "src/SpaceTraders/Database/000_init.sql") ]

open :: IO S.Connection
open = do
  dbConn <- S.open "spacetraders.db"
  S.execute_ dbConn "PRAGMA busy_timeout = 5000;"
  S.execute_ dbConn "PRAGMA cache_size = 10000000;"
  S.execute_ dbConn "PRAGMA foreign_keys = ON;"
  S.execute_ dbConn "PRAGMA journal_mode = WAL;"
  S.execute_ dbConn "PRAGMA synchronous = NORMAL;"
  S.withTransaction dbConn $ do
    version <- getSchemaVersion dbConn `catch` defaultVersion
    mapM_ (S.execute_ dbConn) $ S.Query <$> concatMap (filter (/= "\n") . T.splitOn ";" . T.decodeUtf8) (drop version migrations)
    S.execute_ dbConn "DELETE FROM schema_version;"
    S.execute dbConn "INSERT INTO schema_version (version) VALUES (?);" (S.Only $ length migrations)
  pure dbConn

one_ :: FromJSON a => S.Query -> SpaceTradersT a
one_ q = (\(h:_) -> h) <$> query_ q

query :: (FromJSON a, S.ToRow t) => S.Query -> t -> SpaceTradersT [a]
query q t = do
  env <- ask
  ret <- liftIO $ S.query env.dbConn q t
  return $ mapMaybe (\(h:_) -> decodeText h) ret

query_ :: FromJSON a => S.Query -> SpaceTradersT [a]
query_ q = do
  env <- ask
  ret <- liftIO $ S.query_ env.dbConn q
  return $ mapMaybe (\(h:_) -> decodeText h) ret

wipe :: S.Connection -> IO ()
wipe dbConn = do
  S.withTransaction dbConn $ do
    rows <- S.query_ dbConn "SELECT name FROM sqlite_master WHERE type='table';" :: IO [S.Only T.Text]
    let tables = rows L.\\ [S.Only "schema_version"]
    forM_ tables $ \(S.Only table) -> S.execute_ dbConn $ "DELETE FROM " <> S.Query table <> ";"
