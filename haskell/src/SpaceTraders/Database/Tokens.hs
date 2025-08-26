module SpaceTraders.Database.Tokens
  ( addToken
  , getToken
  ) where

import qualified Data.Text              as T
import qualified Database.SQLite.Simple as S

addToken :: S.Connection -> T.Text -> IO ()
addToken dbConn value = S.execute dbConn "INSERT INTO tokens(data) VALUES (?);" (S.Only value)

getToken :: S.Connection -> IO T.Text
getToken dbConn = do
  [S.Only token] <- S.query_ dbConn "SELECT data FROM tokens;"
  pure token
