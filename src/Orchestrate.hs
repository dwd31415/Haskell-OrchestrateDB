module Orchestrate(Orchestrate.Types.OrchestrateCollection(..),
                   Orchestrate.Types.OrchestrateApplication(..),
                   Orchestrate.REST.validateApplication) where

import qualified Orchestrate.Types
import qualified Orchestrate.REST

createStdApplication :: String -> String -> Orchestrate.Types.OrchestrateApplication
createStdApplication name api_key =
  Orchestrate.Types.OrchestrateApplication {
    Orchestrate.Types.applicationName = name,
    Orchestrate.Types.apiKey = api_key,
    Orchestrate.Types.httpsEndpoint = "https://api.orchestrate.io/v0"
  }
