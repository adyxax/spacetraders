module Main (main) where

import           Control.Exception
import           Data.Char                        (isSpace)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           SpaceTraders
import           SpaceTraders.ApiClient.Agent
import           SpaceTraders.ApiClient.Client
import           SpaceTraders.ApiClient.Contracts
import           SpaceTraders.ApiClient.Errors
import           SpaceTraders.ApiClient.Ships
import           SpaceTraders.ApiClient.Systems
import           SpaceTraders.Model.Ship
import           System.IO.Error

defaultLogLevel :: LogLevel
defaultLogLevel = Info

main = do
  tokenFile <- T.readFile ".token" `catch` handleTokenFileNotFound
  let token = T.dropWhileEnd isSpace tokenFile
  env <- newEnv (tokenReq token) defaultLogLevel
  runSpaceTradersT main' env `catch` handleResetHappened
  where
    handleResetHappened :: ResetHappened -> IO ()
    handleResetHappened _ = do
      registerNewAgent
      main
    handleTokenFileNotFound :: IOError -> IO T.Text
    handleTokenFileNotFound e | isDoesNotExistError e = do
                                  registerNewAgent
                                  T.readFile ".token"
                              | otherwise = ioError e
    main' :: SpaceTradersT ()
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

registerNewAgent :: IO ()
registerNewAgent = do
  putStrLn "spacetraders.io reset happened, registering a new agent..."
  tokenFile <- T.readFile ".account-token"
  let token = T.dropWhileEnd isSpace tokenFile
  env <- newEnv (tokenReq token) defaultLogLevel
  registerResponse <- runSpaceTradersT (register "CORSAIRS" "ADYXAX-HASKELL") env
  case registerResponse of
    Left e             -> throwIO e
    Right registerData -> T.writeFile ".token" registerData.token
