{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orchestrate.REST
    (
      validateApplication,
      orchestrateCollectionPutWithoutKey,
      orchestrateCollectionDelete,
      orchestrateCollectionGet
    ) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Status

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

orchestrateCollectionPutWithoutKey :: ToJSON obj => OrchestrateApplication -> OrchestrateCollection -> obj -> IO Bool
orchestrateCollectionPutWithoutKey application collection object = do
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

orchestrateCollectionGet :: FromJSON res => OrchestrateApplication -> OrchestrateCollection -> String -> IO (Maybe res)
orchestrateCollectionGet application collection key = do
  let api_key = apiKey application
  if api_key == ""
    then return Nothing
    else do
      let url = httpsEndpoint application ++ "/" ++ collectionName collection ++ "/" ++ key
      case parseUrl url of
        Nothing -> return Nothing
        Just unsecuredRequest -> withManager $ \manager -> do
                let request = unsecuredRequest {
                                        method = "GET",
                                        secure = True }
                let reqHead = applyBasicAuth (B.pack $ apiKey application) "" request
                resOrException <- X.try (httpLbs reqHead manager)
                case resOrException of
                  Right res -> if responseStatus res == ok200
                    then do
                      let resBody = responseBody res
                      let jsonObject = decode resBody
                      return jsonObject
                    else return  Nothing
                  Left (_::X.SomeException) -> return  Nothing

orchestrateCollectionDelete :: OrchestrateApplication -> OrchestrateCollection -> IO Bool
orchestrateCollectionDelete application collection = do
  let api_key = apiKey application
  if api_key == ""
  then return False
  else do
    let url = httpsEndpoint application ++ "/" ++ collectionName collection ++ "?force=true"
    case parseUrl url of
      Nothing -> return False
      Just unsecuredRequest -> withManager $ \manager -> do
                  let request = unsecuredRequest {
                                          method = "DELETE",
                                          secure = True }
                  let reqHead = applyBasicAuth (B.pack $ apiKey application) "" request
                  resOrException <- X.try (http reqHead manager)
                  case resOrException of
                    Right res -> if responseStatus res == status204
                      then return True
                      else return False
                    Left (_::X.SomeException) -> return False
