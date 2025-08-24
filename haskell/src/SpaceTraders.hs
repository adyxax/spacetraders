module SpaceTraders
  ( ask
  , Env(..)
  , liftIO
  , logJSON
  , LogLevel(..)
  , newEnv
  , runSpaceTradersT
  , SpaceTradersT
  ) where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.IORef
import qualified Data.Text                  as T
import           Data.Time.Clock            (getCurrentTime)
import           GHC.Generics
import           Network.HTTP.Simple
import           SpaceTraders.Model.Agent
import           System.IO                  (stdout)

data Env = Env
  { agent    :: IORef Agent
  , logLevel :: LogLevel
  , request  :: Request }

type SpaceTradersT a = ReaderT Env IO a

data LogLevel = Error | Warning | Info | Debug deriving (Eq, Generic, Ord, Show)

instance ToJSON LogLevel

logJSON :: ToJSON a => LogLevel -> T.Text -> a -> SpaceTradersT ()
logJSON logLevel message payload = do
  env <- ask
  when (logLevel <= env.logLevel) $ do
    now <- liftIO getCurrentTime
    let v = object
          [ "data"    .= payload
          , "level"   .= logLevel
          , "message" .= message
          , "ts"      .= now
          ]
    liftIO . BL8.hPutStrLn stdout $ encode v

newEnv :: Request -> LogLevel -> IO Env
newEnv req logLevel = do
  agent <- newIORef nullAgent
  pure $ Env agent logLevel req

runSpaceTradersT :: SpaceTradersT a -> Env -> IO a
runSpaceTradersT = runReaderT
