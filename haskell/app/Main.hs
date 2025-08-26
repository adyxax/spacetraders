module Main (main) where

import           Control.Exception
import           Control.Monad
import           Data.Aeson
import           Data.Char                        (isSpace)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import qualified Data.Text.IO                     as T
import qualified Database.SQLite.Simple           as S
import           Network.HTTP.Simple
import           SpaceTraders
import           SpaceTraders.ApiClient.Agent
import           SpaceTraders.ApiClient.Client
import           SpaceTraders.ApiClient.Contracts
import           SpaceTraders.ApiClient.Errors
import           SpaceTraders.ApiClient.Ships
import           SpaceTraders.ApiClient.Systems
import           SpaceTraders.Database
import           SpaceTraders.Database.Tokens
import           SpaceTraders.Model.Ship
import           System.IO.Error

defaultLogLevel :: LogLevel
defaultLogLevel = Info

main = do
  dbConn <- open
  token <- getToken dbConn `catch` (handleTokenNotFound dbConn)
  env <- newEnv dbConn defaultLogLevel $ requestTemplate token
  restart <- runSpaceTradersT main' env `catch` (handleResetHappened dbConn)
  close dbConn
  when restart main
  where
    handleResetHappened :: S.Connection -> ResetHappened -> IO Bool
    handleResetHappened dbConn _ = do
      wipe dbConn
      pure True
    handleTokenNotFound :: S.Connection -> SomeException -> IO T.Text
    handleTokenNotFound dbConn _ = do
      registerNewAgent dbConn
      getToken dbConn
    main' :: SpaceTradersT Bool
    main' = do
      (Right agent) <- myAgent
      logJSON Info "agent" agent
      (Right (contract:[])) <- myContracts
      logJSON Info "contract" contract
      _ <- accept contract
      ships <- myShips
      case ships of
        Left e      -> liftIO $ throwIO e
        Right (s1:s2:_) -> do
          logJSON Info "s1" s1
          (Right s1') <- orbit s1
          logJSON Info "s1 after orbit" s1'.nav.status
          (Right s1'') <- dock s1'
          logJSON Info "s1 after dock" s1''.nav.status
          system <- getSystem s1.nav.systemSymbol
          logJSON Info "system" system
      pure False

registerNewAgent :: S.Connection -> IO T.Text
registerNewAgent dbConn = do
  putStrLn "spacetraders.io reset happened, registering a new agent..."
  tokenFile <- T.readFile ".account-token"
  let token = T.dropWhileEnd isSpace tokenFile
  env <- newEnv dbConn defaultLogLevel $ requestTemplate token
  registerResponse <- runSpaceTradersT (register "CORSAIRS" "ADYXAX-HASKELL") env
  case registerResponse of
    Left e             -> throwIO e
    Right registerData -> do
      addToken dbConn registerData.token
      S.execute dbConn "INSERT INTO agents(data) VALUES (?);" (S.Only $ encode registerData.agent)
      pure registerData.token

requestTemplate :: T.Text -> Request
requestTemplate token = setRequestHost "api.spacetraders.io"
                      $ setRequestPort 443
                      $ setRequestSecure True
                      $ setRequestHeader "Content-Type" ["application/json"]
                      $ setRequestHeader "Authorization" [T.encodeUtf8 $ "Bearer " <> token]
                      $ setRequestBody "{}"
                      $ defaultRequest
