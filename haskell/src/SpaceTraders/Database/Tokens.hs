module SpaceTraders.Database.Tokens
  ( addToken
  , getToken
  ) where

import           Control.Monad.Reader
import qualified Data.Text              as T
import qualified Database.SQLite.Simple as S

import           SpaceTraders
import           SpaceTraders.Utils

addToken :: (HasDatabaseConn env, MonadIO m, MonadReader env m) => T.Text -> m ()
addToken value = execute "INSERT INTO tokens(data) VALUES (?);" (S.Only value)

getToken :: (HasDatabaseConn env, MonadFail m, MonadIO m, MonadReader env m) => m T.Text
getToken = do
  env <- ask
  [[token]] <- liftIO $ S.query_ (getConn env) "SELECT data FROM tokens;"
  return token
