{-# LANGUAGE OverloadedStrings #-}

module SpaceTraders
  ( SpaceTradersT
  , runSpaceTradersT
  , Env(..)
  , HasDatabaseConn
  , HasRequest
  , ask
  , getConn
  , getRequest
  , liftIO
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Database.SQLite.Simple as S
import Network.HTTP.Simple

type SpaceTradersT a = ReaderT Env IO a

runSpaceTradersT :: SpaceTradersT a -> Env -> IO a
runSpaceTradersT = runReaderT

data Env = Env { envConn :: S.Connection
               , envRequest :: Request }

class HasDatabaseConn a where
  getConn :: a -> S.Connection
instance HasDatabaseConn S.Connection where
  getConn = id
instance HasDatabaseConn Env where
  getConn = envConn

class HasRequest a where
  getRequest :: a -> Request
instance HasRequest Env where
  getRequest = envRequest
