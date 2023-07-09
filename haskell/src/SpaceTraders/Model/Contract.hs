{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders.Model.Contract
  ( Contract(..)
  , Delivery(..)
  , Payment(..)
  , Terms(..)
  ) where

import Data.Aeson
import Data.Time
import GHC.Generics
import qualified Data.Text as T

data Contract = Contract { accepted :: Bool
                         , contractId :: T.Text
                         , contractType :: T.Text
                         , expiration :: UTCTime
                         , deadlineToAccept :: UTCTime
                         , factionSymbol :: T.Text
                         , fulfilled :: Bool
                         , terms :: Terms
                         } deriving (Generic, Show)
instance FromJSON Contract where
  parseJSON = withObject "Contract" $ \o ->
    Contract <$> o .: "accepted"
             <*> o .: "id"
             <*> o .: "type"
             <*> o .: "expiration"
             <*> o .: "deadlineToAccept"
             <*> o .: "factionSymbol"
             <*> o .: "fulfilled"
             <*> o .: "terms"
instance ToJSON Contract where
  toEncoding (Contract a i ty e d fa fu te) = pairs ( "accepted" .= a
                                                   <> "id" .= i
                                                   <> "type" .= ty
                                                   <> "expiration" .= e
                                                   <> "deadlineToAccept" .= d
                                                   <> "factionSymbol" .= fa
                                                   <> "fulfilled" .= fu
                                                   <> "terms" .= te )

data Delivery = Delivery { destinationSymbol :: T.Text
                         , tradeSymbol :: T.Text
                         , unitsFulfilled :: Int
                         , unitsRequired :: Int
                         } deriving (FromJSON, Generic, Show, ToJSON)

data Payment = Payment { onAccepted :: Int
                       , onFulfilled :: Int
                       } deriving (FromJSON, Generic, Show, ToJSON)

data Terms = Terms { deadline :: UTCTime
                   , deliver :: [Delivery]
                   , payment :: Payment
                   } deriving (FromJSON, Generic, Show, ToJSON)
