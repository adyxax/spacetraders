{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SpaceTraders.Database.Tokens
  ( getToken
  , setToken
  ) where

import qualified Database.SQLite.Simple as S
import qualified Data.Text as T

getToken :: S.Connection -> IO (T.Text)
getToken conn = do
  [[token]] <- S.query_ conn "SELECT data FROM tokens;"
  return token

setToken :: S.Connection -> T.Text -> IO ()
setToken conn value = S.execute conn "INSERT INTO tokens(data) VALUES (?);" (S.Only value)
