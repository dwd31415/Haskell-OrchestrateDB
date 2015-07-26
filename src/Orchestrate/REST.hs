{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orchestrate.REST
    (
      validateApplication,
      orchestrateCollectionPutWithoutKey,
      orchestrateCollectionDelete,
      orchestrateCollectionGet,
      orchestrateCollectionPut,
      orchestrateCollectionDeleteKey,
      orchestrateCollectionSearch,
      orchestrateCollectionSearchWithOffset,
      parseQueryResponseBody
    ) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Status

import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.ByteString.Char8 as B
import qualified Control.Exception.Lifted as X

import Data.Aeson
import Data.Aeson.Types
import Orchestrate.Types
import Data.Maybe

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

-- KEY/VALUE

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

orchestrateCollectionPut :: ToJSON obj => OrchestrateApplication -> OrchestrateCollection -> String -> obj -> IO Bool
orchestrateCollectionPut application collection key object = do
  let objAsJson = encode object
  let api_key = apiKey application
  if api_key == ""
  then return False
  else do
    let url = httpsEndpoint application ++ "/" ++ collectionName collection ++ "/" ++ key
    case parseUrl url of
      Nothing -> return False
      Just unsecuredRequest -> withManager $ \manager -> do
                  let request = unsecuredRequest {
                                          method = "PUT",
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

orchestrateCollectionDeleteKey :: OrchestrateApplication -> OrchestrateCollection -> String -> IO Bool
orchestrateCollectionDeleteKey application collection key = do
  let api_key = apiKey application
  if api_key == ""
  then return False
  else do
    let url = httpsEndpoint application ++ "/" ++ collectionName collection ++ "/" ++ key ++ "?force=true"
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

-- SEARCH

orchestrateCollectionSearch :: {-FromJSON res =>-} OrchestrateApplication -> OrchestrateCollection -> String -> IO (Maybe([Object],Bool))
orchestrateCollectionSearch application collection query = orchestrateCollectionSearchWithOffset application collection query 0 10


orchestrateCollectionSearchWithOffset :: {-FromJSON res =>-} OrchestrateApplication -> OrchestrateCollection -> String -> Integer -> Integer -> IO (Maybe([Object],Bool))
orchestrateCollectionSearchWithOffset application collection query offset limit = do
  let api_key = apiKey application
  if api_key == ""
    then return Nothing
    else do
      let url = httpsEndpoint application ++ "/" ++ collectionName collection ++ "?query=" ++ query ++ "&limit=" ++ show limit ++ "&offset=" ++ show offset
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
                      let (results,count) = fromMaybe ([],0) (parseQueryResponseBody resBody)
                      return $ Just (results,count > (fromIntegral (length results) + offset))

                    else return  Nothing
                  Left (_::X.SomeException) -> return  Nothing

parseQueryResponseBody :: BSLazy.ByteString -> Maybe ([Object],Integer)
parseQueryResponseBody resBodyRaw = do
  result <- decode resBodyRaw
  (count,results) <- flip parseMaybe result $ \obj -> do
                         count <- obj .: "total_count" :: Parser Integer
                         results <- obj .: "results" :: Parser [OrchestrateQueryResult]
                         return (count,results)
  return (resultValuesAsList results,count)
