{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders
  ( SpaceTradersT
  , runSpaceTradersT
  , Config(..)
  , ask
  , liftIO
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Database.SQLite.Simple as S
import qualified Data.Text as T

type SpaceTradersT a = ReaderT Config IO a

runSpaceTradersT :: SpaceTradersT a -> Config -> IO a
runSpaceTradersT = runReaderT

data Config = Config { conn :: S.Connection
                     , token :: T.Text
                     }
