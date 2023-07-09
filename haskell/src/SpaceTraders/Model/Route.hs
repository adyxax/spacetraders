{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Model.Route
  ( Route(..)
  , RouteEndpoint(..)
  ) where

import Data.Aeson
import Data.Time
import GHC.Generics
import qualified Data.Text as T

data Route = Route { arrival :: UTCTime
                   , departure :: RouteEndpoint
                   , departureTime :: UTCTime
                   , destination :: RouteEndpoint
                   } deriving (FromJSON, Generic, Show, ToJSON)

data RouteEndpoint = RouteEndpoint { routeEndpointType :: T.Text
                                   , symbol :: T.Text
                                   , systemSymbol :: T.Text
                                   , x :: Int
                                   , y :: Int
                                   } deriving (Generic, Show)
instance FromJSON RouteEndpoint where
  parseJSON = withObject "RouteEndpoint" $ \o ->
    RouteEndpoint <$> o .: "type"
                  <*> o .: "symbol"
                  <*> o .: "systemSymbol"
                  <*> o .: "x"
                  <*> o .: "y"
instance ToJSON RouteEndpoint where
  toEncoding (RouteEndpoint t s ss xx yy) = pairs ( "type" .= t
                                                 <> "symbol" .= s
                                                 <> "systemSymbol" .= ss
                                                 <> "x" .= xx
                                                 <> "y" .= yy )
