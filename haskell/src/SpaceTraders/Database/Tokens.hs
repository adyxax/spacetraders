{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SpaceTraders.Database.Tokens
  ( addToken
  , getToken
  ) where

import Control.Monad.Reader
import qualified Database.SQLite.Simple as S
import qualified Data.Text as T

import SpaceTraders

addToken :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => T.Text -> m ()
addToken value = do
  env <- ask
  liftIO $ S.execute (getConn env) "INSERT INTO tokens(data) VALUES (?);" (S.Only value)

getToken :: (HasDatabaseConn env, MonadFail m, MonadIO m, MonadReader env m) => m T.Text
getToken = do
  env <- ask
  [[token]] <- liftIO $ S.query_ (getConn env) "SELECT data FROM tokens;"
  return token
