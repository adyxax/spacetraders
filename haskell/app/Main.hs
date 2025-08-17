module Main (main) where

import           Control.Exception
import           Data.Char                     (isSpace)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           SpaceTraders
import           SpaceTraders.ApiClient.Agent
import           SpaceTraders.ApiClient.Client
import           SpaceTraders.ApiClient.Errors

main = do
  tokenFile <- T.readFile ".token"
  let token = T.dropWhileEnd isSpace tokenFile
  env <- newEnv $ tokenReq token
  runSpaceTradersT main' env
  where
    main' :: SpaceTradersT ()
    main' = do
      agent <- myAgent
      case agent of
        Left (ApiResetHappened _) -> liftIO $ do
          putStrLn "spacetraders.io reset happened, registering a new agent..."
          tokenFile <- T.readFile ".account-token"
          let token = T.dropWhileEnd isSpace tokenFile
          env <- newEnv $ tokenReq token
          registerResponse <- runSpaceTradersT (register "CORSAIRS" "ADYXAX-HASKELL") env
          case registerResponse of
            Left e             -> throwIO e
            Right registerData -> T.writeFile ".token" registerData.token
          main
        Left e                    -> liftIO $ throwIO e
        Right agent               -> liftIO . print $ show agent
