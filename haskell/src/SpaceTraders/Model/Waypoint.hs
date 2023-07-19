{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Model.Waypoint
  ( Waypoint(..)
  ) where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Waypoint = Waypoint { symbol :: T.Text
                         , waypointType :: T.Text
                         , x :: Int
                         , y :: Int
                         } deriving (Generic, Show)
instance FromJSON Waypoint where
  parseJSON = withObject "Waypoint" $ \o ->
    Waypoint <$> o .: "symbol"
             <*> o .: "type"
             <*> o .: "x"
             <*> o .: "y"
instance ToJSON Waypoint where
  toJSON (Waypoint s t xx yy) = object [ "symbol" .= s
                                       , "type" .= t
                                       , "x" .= xx
                                       , "y" .= yy ]
