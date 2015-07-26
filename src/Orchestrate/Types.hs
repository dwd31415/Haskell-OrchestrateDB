{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TypeFamilies           #-}

module Orchestrate.Types
    ( OrchestrateApplication(..),
      OrchestrateCollection(..),
      OrchestrateQueryResult(..),
      OrchestratePath (..),
      resultValuesAsList) where

import Data.Aeson
import GHC.Generics
import Control.Applicative        ((<$>), (<*>))
import Control.Monad

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

data OrchestratePath = OrchestratePath {
  collection :: String,
  kind :: String,
  key :: String,
  ref :: String,
  reftimePath :: Integer
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

data OrchestrateQueryResult = OrchestrateQueryResult {
  path :: OrchestratePath,
  value :: Object,
  score :: Double,
  reftimeQueryResult :: Integer
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

resultValuesAsList :: [OrchestrateQueryResult] -> [Object]
resultValuesAsList = map value
