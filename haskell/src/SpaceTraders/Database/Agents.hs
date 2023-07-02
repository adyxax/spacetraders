{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module SpaceTraders.Database.Agents
  ( setAgent
  ) where

import Data.Aeson
import qualified Database.SQLite.Simple as S

import SpaceTraders.Model.Agent

setAgent :: S.Connection -> Agent -> IO ()
setAgent conn agent = S.execute conn "INSERT INTO agents(data) VALUES (json(?));" (S.Only (encode agent))
