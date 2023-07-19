{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Contracts
  ( addContract
  , getContracts
  , setContract
  , updateContract
  ) where

import Control.Monad.Reader
import Data.Aeson
import Data.Maybe
import qualified Database.SQLite.Simple as S

import SpaceTraders
import SpaceTraders.Model.Contract
import SpaceTraders.Utils

addContract :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Contract -> m ()
addContract contract = do
  env <- ask
  liftIO $ S.execute (getConn env) "INSERT INTO contracts(data) VALUES (json(?));" (S.Only (encode contract))

getContracts :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => m [Contract]
getContracts = do
  env <- ask
  ret <- liftIO $ S.query_ (getConn env) "SELECT data FROM contracts;"
  return . catMaybes $ map (decodeText . head) ret

setContract :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Contract -> m ()
setContract contract = do
  env <- ask
  count <- liftIO (S.query (getConn env) "SELECT count(id) FROM contracts WHERE data->>'contractId' = ?;" (S.Only $ contractId contract) :: IO [[Int]])
  if count == [[0]] then addContract contract
                    else updateContract contract

updateContract :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => Contract -> m ()
updateContract contract = do
  env <- ask
  liftIO $ S.execute (getConn env) "UPDATE contracts SET data = json(?) WHERE data->>'contractId' = ?;" (encode contract, contractId contract)
