{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Contracts
  ( addContract
  , getContracts
  , setContract
  , updateContract
  ) where

import Control.Monad.Reader
import Data.Aeson
import qualified Database.SQLite.Simple as S

import SpaceTraders
import SpaceTraders.Model.Contract
import SpaceTraders.Utils

addContract :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Contract -> m ()
addContract contract = execute "INSERT INTO contracts(data) VALUES (json(?));" (S.Only (encode contract))

getContracts :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => m [Contract]
getContracts = query_ "SELECT data FROM contracts;"

setContract :: (HasDatabaseConn env, MonadFail m, MonadIO m, MonadReader env m) => Contract -> m ()
setContract contract = do
  c <- count "SELECT count(id) FROM contracts WHERE data->>'contractId' = ?;" (S.Only $ contractId contract)
  if c == 0 then addContract contract
            else updateContract contract

updateContract :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Contract -> m ()
updateContract contract = execute "UPDATE contracts SET data = json(?) WHERE data->>'contractId' = ?;" (encode contract, contractId contract)
