module Orchestrate.Types
    ( OrchestrateApplication(..),
      OrchestrateCollection(..) ) where

data OrchestrateApplication = OrchestrateApplication {
    apiKey :: String,
    httpsEndpoint :: String
}

data OrchestrateCollection = OrchestrateCollection {
    name :: String
}
