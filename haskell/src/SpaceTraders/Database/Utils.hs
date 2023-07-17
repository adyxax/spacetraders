module SpaceTraders.Database.Utils
  ( decodeText
  ) where

import Data.Aeson
import Data.ByteString.Builder(toLazyByteString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8Builder)

decodeText :: FromJSON a => T.Text -> Maybe a
decodeText = decode . toLazyByteString . encodeUtf8Builder
