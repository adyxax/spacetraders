{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Contracts
  ( addContract
  ) where

import Data.Aeson
import qualified Database.SQLite.Simple as S

import SpaceTraders.Model.Contract

addContract :: S.Connection -> Contract -> IO ()
addContract conn contract = S.execute conn "INSERT INTO contracts(data) VALUES (json(?));" (S.Only (encode contract))
