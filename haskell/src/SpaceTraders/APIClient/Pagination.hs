{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module SpaceTraders.APIClient.Pagination
  ( Pagination(..)
  , nextPage
  ) where

import           Data.Aeson
import           GHC.Generics

data Pagination = Pagination { limit :: Int
                             , page  :: Int
                             , total :: Int
                             } deriving (FromJSON, Generic, Show)

nextPage :: Pagination -> Pagination
nextPage (Pagination l p t) = Pagination l (p + 1) t
