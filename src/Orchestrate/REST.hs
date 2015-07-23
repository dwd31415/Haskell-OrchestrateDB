module Orchestrate.REST
    (
      validateApplication
    ) where

import Network.HTTP
import Network.HTTP.Conduit

import Orchestrate.Types

validateApplication :: OrchestrateApplication -> IO Bool
validateApplication application = do
  let api_key = apiKey application
  if api_key == ""
    then return False
    else return True
