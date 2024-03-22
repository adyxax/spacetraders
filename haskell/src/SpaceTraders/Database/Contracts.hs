{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Contracts
  ( getContracts
  , setContract
  ) where

import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Data.Aeson
import qualified Database.SQLite.Simple      as S

import           SpaceTraders
import           SpaceTraders.Model.Contract
import           SpaceTraders.Utils

getContracts :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => m [Contract]
getContracts = query_ "SELECT data FROM contracts;"

setContract :: (HasDatabaseConn env, MonadError e m, MonadIO m, MonadReader env m) => Contract -> m ()
setContract contract = updateContract `catchError` addContract
  where
    addContract _ = execute "INSERT INTO contracts(data) VALUES (json(?));" (S.Only (encode contract))
    updateContract = execute "UPDATE contracts SET data = json(?) WHERE data->>'contractId' = ?;" (encode contract, contractId contract)
