module SpaceTraders.Utils
  ( count
  , decodeText
  , execute
  , fromJSONValue
  , int2ByteString
  , one_
  , query
  , query_
  ) where

import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as B
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.SQLite.Simple as S

import SpaceTraders

decodeText :: FromJSON a => T.Text -> Maybe a
decodeText = decode . B.toLazyByteString . T.encodeUtf8Builder

execute :: (HasDatabaseConn env, MonadReader env m, MonadIO m, S.ToRow t) => S.Query -> t -> m ()
execute q t = do
  env <- ask
  liftIO $ S.execute (getConn env) q t

fromJSONValue :: FromJSON a => Value -> Either String a
fromJSONValue = parseEither parseJSON

int2ByteString :: Int -> B.ByteString
int2ByteString = B.pack . map B.c2w . show

one_ :: (FromJSON b, HasDatabaseConn env, MonadReader env m, MonadIO m) => S.Query -> m b
one_ q = query_ q >>= pure . head

count :: (HasDatabaseConn env, MonadFail m, MonadReader env m, MonadIO m, S.ToRow t) => S.Query -> t -> m Int
count q t = do
  env <- ask
  [[ret]] <- liftIO (S.query (getConn env) q t :: IO [[Int]])
  return ret

query :: (FromJSON b, HasDatabaseConn env, MonadReader env m, MonadIO m, S.ToRow t) => S.Query -> t -> m [b]
query q t = do
  env <- ask
  ret <- liftIO $ S.query (getConn env) q t
  return . catMaybes $ map (decodeText . head) ret

query_ :: (FromJSON b, HasDatabaseConn env, MonadReader env m, MonadIO m) => S.Query -> m [b]
query_ q = do
  env <- ask
  ret <- liftIO $ S.query_ (getConn env) q
  return . catMaybes $ map (decodeText . head) ret
