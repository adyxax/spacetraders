{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Contracts
  ( addContract
  ) where

import Control.Monad.Reader
import Data.Aeson
import qualified Database.SQLite.Simple as S

import SpaceTraders
import SpaceTraders.Model.Contract

addContract :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Contract -> m ()
addContract contract = do
  env <- ask
  liftIO $ S.execute (getConn env) "INSERT INTO contracts(data) VALUES (json(?));" (S.Only (encode contract))
