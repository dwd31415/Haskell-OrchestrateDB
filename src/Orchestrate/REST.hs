{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orchestrate.REST
    (
      validateApplication,
      orchestrateCollectionPut
    ) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.ByteString.Char8 as B
import qualified Control.Exception.Lifted as X

import Data.Aeson
import Orchestrate.Types

validateApplication :: OrchestrateApplication -> IO Bool
validateApplication application = do
  let api_key = apiKey application
  if api_key == ""
    then return False
    else do
      let url = httpsEndpoint application
      case parseUrl url of
        Nothing -> return False
        Just unsecuredRequest -> withManager $ \manager -> do
                    let request = applyBasicAuth (B.pack $ apiKey application) "" unsecuredRequest
                    let reqHead = request {
                                            method = "HEAD",
                                            secure = True }
                    resOrException <- X.try (http reqHead manager)
                    case resOrException of
                      Right res -> if responseStatus res == ok200
                        then return True
                        else return False
                      Left (_::X.SomeException) -> return False

orchestrateCollectionPut :: ToJSON obj => OrchestrateApplication -> OrchestrateCollection -> obj -> IO Bool
orchestrateCollectionPut application collection object = do
  let objAsJson = encode object
  let api_key = apiKey application
  if api_key == ""
  then return False
  else do
    let url = httpsEndpoint application ++ "/" ++ collectionName collection
    case parseUrl url of
      Nothing -> return False
      Just unsecuredRequest -> withManager $ \manager -> do
                  let request = unsecuredRequest {
                                          method = "POST",
                                          secure = True,
                                          requestHeaders = [("Content-Type", "application/json")],
                                          requestBody = RequestBodyBS $ BSLazy.toStrict objAsJson }
                  let reqHead = applyBasicAuth (B.pack $ apiKey application) "" request
                  resOrException <- X.try (http reqHead manager)
                  case resOrException of
                    Right res -> if responseStatus res == status201
                      then return True
                      else return False
                    Left (_::X.SomeException) -> return False
