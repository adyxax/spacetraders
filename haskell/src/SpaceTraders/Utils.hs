module SpaceTraders.Utils
  ( decodeText
  , fromJSONValue
  , int2ByteString
  ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

decodeText :: FromJSON a => T.Text -> Maybe a
decodeText = decode . B.toLazyByteString . T.encodeUtf8Builder

fromJSONValue :: FromJSON a => Value -> Either String a
fromJSONValue = parseEither parseJSON

int2ByteString :: Int -> B.ByteString
int2ByteString = B.pack . map B.c2w . show
