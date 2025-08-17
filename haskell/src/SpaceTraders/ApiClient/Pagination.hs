module SpaceTraders.ApiClient.Pagination
  ( firstPage
  , isLastPage
  , nextPage
  , Pagination(..)
  ) where

import           Data.Aeson
import           Data.Maybe
import           GHC.Generics

firstPage :: Pagination
firstPage = Pagination{limit=20, page=1, total=0}

isLastPage :: Maybe Pagination -> Bool
isLastPage (Just (Pagination l p t)) = l * p >= t
isLastPage Nothing                   = True

nextPage :: Pagination -> Pagination
nextPage (Pagination l p t) = Pagination l (p + 1) t

data Pagination = Pagination
  { limit :: Int
  , page  :: Int
  , total :: Int
  } deriving (Generic, Show)

instance FromJSON Pagination
