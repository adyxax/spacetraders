module SpaceTraders.ApiClient.Contracts
  ( accept
  , myContracts
  ) where

import qualified Data.Text.Encoding            as T
import           Network.HTTP.Simple

import           SpaceTraders
import           SpaceTraders.ApiClient.Client
import           SpaceTraders.Model.Contract

accept :: Contract -> SpaceTradersT Contract
accept contract | contract.accepted = pure contract
                | otherwise = send $ setRequestMethod "POST"
                                   . setRequestPath (T.encodeUtf8 $ mconcat ["/v2/my/contracts/", contract.id, "/accept"])

myContracts :: SpaceTradersT [Contract]
myContracts = send $ setRequestPath "/v2/my/contracts"
