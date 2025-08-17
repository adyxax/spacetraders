module SpaceTraders
  ( ask
  , Env(..)
  , liftIO
  , newEnv
  , runSpaceTradersT
  , SpaceTradersT
  ) where

import           Control.Monad.Reader
import           Data.IORef
import           Network.HTTP.Simple
import           SpaceTraders.Model.Agent

data Env = Env
  { agent   :: IORef Agent
  , request :: Request }

type SpaceTradersT a = ReaderT Env IO a

newEnv :: Request -> IO Env
newEnv req = do
  agent <- newIORef nullAgent
  pure $ Env agent req

runSpaceTradersT :: SpaceTradersT a -> Env -> IO a
runSpaceTradersT = runReaderT
