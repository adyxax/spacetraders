{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Database.Systems
  ( addSystems
  , getSystems
  ) where

import Data.Aeson
import Data.Maybe
import qualified Database.SQLite.Simple as S

import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder(toLazyByteString)

import SpaceTraders.Model.System

addSystems :: S.Connection -> [System] -> IO ()
addSystems conn systems = S.withTransaction conn $ S.executeMany conn "INSERT INTO systems(data) VALUES (json(?));" $ S.Only <$> map encode systems

getSystems :: S.Connection -> IO [System]
getSystems conn = do
  ret <- S.query_ conn "SELECT data from systems;"
  return . catMaybes $ map (decode . toLazyByteString . encodeUtf8Builder . head) ret
