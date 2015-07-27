{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Orchestrate.Types
    ( OrchestrateApplication(..),
      OrchestrateCollection(..),
      OrchestrateQueryResult(..),
      OrchestratePath (..),
      OrchestrateListResult (..),
      resultValuesAsList) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad
import           Data.Aeson
import           GHC.Generics

{-|
A data type, that represents an Orchestrate application. It stores an api key (generated online), and a https-endpoint..
-}
data OrchestrateApplication = OrchestrateApplication {
    applicationName :: String,
    apiKey          :: String,
    httpsEndpoint   :: String
}

{-|
Represents a collection inside an OrchestrateApplication, it stores all data necessary to access it.
-}
data OrchestrateCollection = OrchestrateCollection {
    collectionName :: String
}

-- |TypeClass for OrchestrateQueryResult and OrchestrateListResult, it makes it possible to have one function('resultValuesAsList'), that extractes
-- the values from both of those types.
class OrchestrateIntermediateResult a where
  resultValuesAsList :: [a] -> [Object]

-- |Represents a path, as it is returned by the LIST and SEARCH function.
-- Right now it is not actually used, but it might be useful in the future.
data OrchestratePath = OrchestratePath {
  orchestratePathCollection :: String,
  orchestratePathKind       :: String,
  orchestratePathKey        :: String,
  orchestratePathRef        :: String,
  orchestratePathReftime    :: Integer
}  deriving (Show,Read,Generic)

instance FromJSON OrchestratePath where
    parseJSON (Object v) = OrchestratePath <$>
                           v .: "collection"  <*>
                           v .: "kind" <*>
                           v .: "key" <*>
                           v .: "ref" <*>
                           v .: "reftime"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero
instance ToJSON   OrchestratePath

-- | Haskell type for the JSON scheme, that is returned by the SEARCH function.
data OrchestrateQueryResult = OrchestrateQueryResult {
  orchestrateQueryResultPath               :: OrchestratePath,
  orchestrateQueryResultValue              :: Object,
  orchestrateQueryResultScore              :: Double,
  orchestrateQueryResultReftimeQueryResult :: Integer
}  deriving (Show,Generic)

instance FromJSON OrchestrateQueryResult where
    parseJSON (Object v) = OrchestrateQueryResult <$>
                           v .: "path"  <*>
                           v .: "value" <*>
                           v .: "score" <*>
                           v .: "reftime"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero
instance ToJSON   OrchestrateQueryResult
instance OrchestrateIntermediateResult OrchestrateQueryResult where
  resultValuesAsList = map orchestrateQueryResultValue


-- | Haskell type for the JSON scheme, that is returned by the LIST function.
data OrchestrateListResult = OrchestrateListResult {
  orchestrateListResultPath               :: OrchestratePath,
  orchestrateListResultValue              :: Object,
  orchestrateListResultReftimeQueryResult :: Integer
}  deriving (Show,Generic)

instance FromJSON OrchestrateListResult where
    parseJSON (Object v) = OrchestrateListResult <$>
                           v .: "path"  <*>
                           v .: "value" <*>
                           v .: "reftime"
    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero
instance ToJSON   OrchestrateListResult
instance OrchestrateIntermediateResult OrchestrateListResult where
  resultValuesAsList = map orchestrateListResultValue
