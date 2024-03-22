module SpaceTraders
  ( SpaceTradersT
  , microSinceEpoch
  , newEnv
  , runSpaceTradersT
  , Env(..)
  , HasDatabaseConn
  , HasRequest
  , ask
  , getConn
  , getLastAPICall
  , getRequest
  , liftIO
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.IORef
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import qualified Database.SQLite.Simple as S
import           Network.HTTP.Simple

type SpaceTradersT a = ReaderT Env IO a

newEnv :: S.Connection -> Request -> IO Env
newEnv conn req = do
  r <- newIORef 0
  return $ Env conn r req

runSpaceTradersT :: SpaceTradersT a -> Env -> IO a
runSpaceTradersT = runReaderT

microSinceEpoch :: IO Integer
microSinceEpoch = do
  t <- getCurrentTime
  return $ floor . (1e6 *) . nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds t

data Env = Env { envConn        :: S.Connection
               , envLastAPICall :: IORef Integer
               , envRequest     :: Request }

class HasDatabaseConn a where
  getConn :: a -> S.Connection
instance HasDatabaseConn S.Connection where
  getConn = id
instance HasDatabaseConn Env where
  getConn = envConn

class HasRequest a where
  getLastAPICall :: a -> IORef Integer
  getRequest :: a -> Request
instance HasRequest Env where
  getLastAPICall = envLastAPICall
  getRequest = envRequest
