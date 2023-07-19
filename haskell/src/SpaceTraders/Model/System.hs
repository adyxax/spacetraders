{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Model.System
  ( System(..)
  ) where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

import SpaceTraders.Model.Waypoint(Waypoint)

data System = System { sectorSymbol :: T.Text
                     , symbol :: T.Text
                     , systemType :: T.Text
                     , x :: Int
                     , y :: Int
                     , waypoints :: [Waypoint]
                     --, factions :: [Faction]
                     } deriving (Generic, Show)
instance FromJSON System where
  parseJSON = withObject "System" $ \o ->
    System <$> o .: "sectorSymbol"
           <*> o .: "symbol"
           <*> o .: "type"
           <*> o .: "x"
           <*> o .: "y"
           <*> o .: "waypoints"
instance ToJSON System where
  toJSON (System ss s t xx yy w) = object [ "sectorSymbol" .= ss
                                          , "symbol" .= s
                                          , "type" .= t
                                          , "x" .= xx
                                          , "y" .= yy
                                          , "waypoints" .= w ]
