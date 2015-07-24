module Orchestrate.Types
    ( OrchestrateApplication(..),
      OrchestrateCollection(..) ) where
{-|
A data type, that represents an Orchestrate application. It stores an api key (generated online), and a https-endpoint..
-}
data OrchestrateApplication = OrchestrateApplication {
    applicationName :: String,
    apiKey :: String,
    httpsEndpoint :: String
}

{-|
Represents a collection inside an OrchestrateApplication, it stores all data necessary to access it.
-}
data OrchestrateCollection = OrchestrateCollection {
    collectionName :: String
}
