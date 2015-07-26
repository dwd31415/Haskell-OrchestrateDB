module Orchestrate(Orchestrate.Types.OrchestrateCollection(..),
                   Orchestrate.Types.OrchestrateApplication(..),
                   Orchestrate.REST.validateApplication,
                   Orchestrate.REST.orchestrateCollectionGet,
                   Orchestrate.REST.orchestrateCollectionPut,
                   Orchestrate.REST.orchestrateCollectionPutWithoutKey,
                   Orchestrate.REST.orchestrateCollectionDelete,
                   Orchestrate.REST.orchestrateCollectionDeleteKey,
                   Orchestrate.REST.orchestrateCollectionSearch,
                   Orchestrate.REST.orchestrateCollectionSearchWithOffset,
                   createStdCollection,
                   createStdApplication) where

import qualified Orchestrate.Types
import qualified Orchestrate.REST

createStdApplication :: String -> String -> Orchestrate.Types.OrchestrateApplication
createStdApplication name api_key =
  Orchestrate.Types.OrchestrateApplication {
    Orchestrate.Types.applicationName = name,
    Orchestrate.Types.apiKey = api_key,
    Orchestrate.Types.httpsEndpoint = "https://api.orchestrate.io/v0"
  }

createStdCollection :: String -> Orchestrate.Types.OrchestrateCollection
createStdCollection name = Orchestrate.Types.OrchestrateCollection {
  Orchestrate.Types.collectionName = name
}
