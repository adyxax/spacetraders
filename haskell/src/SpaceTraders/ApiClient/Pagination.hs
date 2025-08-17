module SpaceTraders.ApiClient.Pagination
  ( Pagination(..)
  , isLastPage
  , nextPage
  ) where

import           Data.Aeson
import           Data.Maybe
import           GHC.Generics

data Pagination = Pagination { limit :: Int
                             , page  :: Int
                             , total :: Int
                             } deriving (Generic, Show)

instance FromJSON Pagination

isLastPage :: Maybe Pagination -> Bool
isLastPage (Just (Pagination l p t)) = l * p >= t
isLastPage Nothing                   = True

nextPage :: Pagination -> Pagination
nextPage (Pagination l p t) = Pagination l (p + 1) t
