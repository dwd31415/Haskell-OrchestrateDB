{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Orchestrate.REST
Description : All functions which interact with the Orchestrate.io REST API.
Copyright   : (c) Adrian Dawid 2015
License     : BSD3
Maintainer  : adriandwd@gmail.com
Stability   : stable

This module conatins all the functins which interact with the Orchestrate.io REST API.
Right now these actions are supported:

  *  	Validate API Keys

  *   List Key/Values

  *   Create Key/Value

  *   Create Key/Value with server-generated key

  *   Update Key/Value

  *   Retrieve Value for Key

  *  	Delete Collection(s)

  *   Query Collection

-}
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
      orchestrateCollectionList
    ) where

import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

import qualified Control.Exception.Lifted  as X
import qualified Data.ByteString.Char8     as B
import qualified Data.ByteString.Lazy      as BSLazy

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Orchestrate.Types

validateApplication :: OrchestrateApplication -> IO Bool
-- ^The 'validateApplication' function validates your API key,
-- by making an authenticated HEAD request to the endpoint specified in the 'OrchestrateApplication' record.
-- The function returns False when the key is invalid or the connection could not be established.
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

orchestrateCollectionList :: OrchestrateApplication -> OrchestrateCollection -> Integer -> IO (Maybe [Object])
-- ^ The 'orchestrateCollectionList' function lists the contents of the specified collection, by making GET request to the \/$collection?limit=$limit endpoint.
--   For more information check out the Orchestrate.io API docs: <https://orchestrate.io/docs/apiref#keyvalue-list>.
--   If connecting to the api fails, or the api key stored in the application record is invlaid, 'Nothing' is returned.
--   Otherwise an array of the type 'Object'(see the documentation of 'Data.Aeson' for more information) is returned, it
--   contains the values from the HTTP response(see  <https://orchestrate.io/docs/apiref#keyvalue-list> for an example of how the response looks like in JSON).
--
--   = Example:
--   @
--      let dbApplication = DB.createStdApplication \"APPLICATION_NAME\" \"API_KEY\"
--      let dbCollection = DB.createStdCollection \"COLLECTION_NAME\"
--      dbContents <- DB.orchestrateCollectionList dbApplication dbCollection 10
--   @
orchestrateCollectionList application collection limit = do
  let api_key = apiKey application
  if api_key == ""
    then return Nothing
    else do
      let url = httpsEndpoint application ++ "/" ++ collectionName collection ++ "?limit=" ++ show limit
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
                      let results = fromMaybe [] (parseListResponseBody resBody)
                      return $ Just results

                    else return  Nothing
                  Left (_::X.SomeException) -> return  Nothing

-- KEY/VALUE

orchestrateCollectionPutWithoutKey :: ToJSON obj => OrchestrateApplication -> OrchestrateCollection -> obj -> IO Bool
-- ^ The 'orchestrateCollectionPutWithoutKey' function stores a Haskell value(with a 'ToJSON' instance) in an Orchestrate.io database.
--   It does so by making a POST request to the \/$collection endpoint(Offical API docs:<https://orchestrate.io/docs/apiref#keyvalue-post>).
--   This function does not need a user specified key, because it ueses a server-generated key, if you want to know the key
--   use 'orchestrateCollectionPut' instead of this function.
--
--   = Example:
--   @
--      data TestRecord = TestRecord
--        { string :: String
--         , number :: Int
--        } deriving (Show,Read,Generic,Eq)
--
--      instance FromJSON TestRecord
--      instance ToJSON TestRecord
--
--      let dbApplication = DB.createStdApplication \"APPLICATION_NAME\" \"API_KEY\"
--      let dbCollection = DB.createStdCollection \"COLLECTION_NAME\"
--      let testRecord = TestRecord {string = "You may delay, but time will not!",number = 903}
--      _ <- DB.orchestrateCollectionPutWithoutKey dbApplication dbCollection testRecord
--   @
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
-- ^ The 'orchestrateCollectionPut' function stores a Haskell value(with a 'ToJSON' instance) in an Orchestrate.io database.
--   It does so by making a PUT request to the \/$collection\/$key endpoint(Offical API docs:<https://orchestrate.io/docs/apiref#keyvalue-put>).
--   In order to upload a Haskell Value to the database, it must have an instance of 'ToJSON' because this
--   client uses 'Data.Aeson' to convert those Haskel Values to JSON, which is required by Orchestrate.io.
--
--   = Example:
--   @
--      data TestRecord = TestRecord
--        { string :: String
--         , number :: Int
--        } deriving (Show,Read,Generic,Eq)
--
--      instance FromJSON TestRecord
--      instance ToJSON TestRecord
--
--      let dbApplication = DB.createStdApplication \"APPLICATION_NAME\" \"API_KEY\"
--      let dbCollection = DB.createStdCollection \"COLLECTION_NAME\"
--      let testRecord = TestRecord {string = "You may delay, but time will not!",number = 903}
--      _ <- DB.orchestrateCollectionPutWithoutKey dbApplication dbCollection "KEY" testRecord
--   @
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
-- ^ The 'orchestrateCollectionGet' function request a value from an Orchestrate.io database, and tries to convert it to the specified Haskell type,
--  if either gettings the value from the database or converting it to the Haskell type fails 'Nothing' is returned.
--  The value is requested by making a GET request to the \/$collection\/$key endpoint(Offical documentation:<https://orchestrate.io/docs/apiref#keyvalue-get>)
--
--   = Example:
--   @
--      data TestRecord = TestRecord
--        { string :: String
--         , number :: Int
--        } deriving (Show,Read,Generic,Eq)
--
--      instance FromJSON TestRecord
--      instance ToJSON TestRecord
--
--      let dbApplication = DB.createStdApplication \"APPLICATION_NAME\" \"API_KEY\"
--      let dbCollection = DB.createStdCollection \"COLLECTION_NAME\"
--      let testRecord = TestRecord {string = "You may delay, but time will not!",number = 903}
--      dbValue <- DB.orchestrateCollectionGet dbApplication dbCollection "KEY" :: IO (Maybe TestRecord)
--   @
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
-- ^ The 'orchestrateCollectionDeleteKey' function deletes a value from an Orchestrate.io database.
--  The value is requested by making a DELETE request to the \/$collection\/$key endpoint(Offical documentation:<https://orchestrate.io/docs/apiref#keyvalue-delete>)
--
--   = Example:
--   @
--      data TestRecord = TestRecord
--        { string :: String
--         , number :: Int
--        } deriving (Show,Read,Generic,Eq)
--
--      instance FromJSON TestRecord
--      instance ToJSON TestRecord
--
--      let dbApplication = DB.createStdApplication \"APPLICATION_NAME\" \"API_KEY\"
--      let dbCollection = DB.createStdCollection \"COLLECTION_NAME\"
--      _ <- DB.orchestrateCollectionKey dbApplication dbCollection "KEY"
--   @
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
-- ^ The 'orchestrateCollectionDelete' function deletes a collection from an Orchestrate.io application.
--  The value is requested by making a DELETE request to the \/$collection endpoint(Offical documentation:<https://orchestrate.io/docs/apiref#collections-delete>)
--
--   = Example:
--   @
--      let dbApplication = DB.createStdApplication \"APPLICATION_NAME\" \"API_KEY\"
--      let dbCollection = DB.createStdCollection \"COLLECTION_NAME\"
--      _ <- DB.orchestrateCollectionDelete dbApplication dbCollection
--   @
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

orchestrateCollectionSearch :: OrchestrateApplication -> OrchestrateCollection -> String -> IO (Maybe([Object],Bool))
-- Please see 'orchestrateCollectionSearchWithOffset' for more information. This function just calls it without an offset and with a limit of 10.
orchestrateCollectionSearch application collection query = orchestrateCollectionSearchWithOffset application collection query 0 10


orchestrateCollectionSearchWithOffset :: OrchestrateApplication -> OrchestrateCollection -> String -> Integer -> Integer -> IO (Maybe([Object],Bool))
-- ^ The 'orchestrateCollectionSearchWithOffset' function searches for the query in the database and returns an array
--   of type Maybe ['Object']. Nothing is returned when connecting or authentication fails. The function uses the
--   SEARCH method of the Orchestrate.io API (Offical documentation:<https://orchestrate.io/docs/apiref#search-collection>)
--   , but automatically parsers the response. It returns a tupel of the type (Maybe(['Object'],Bool)), the boolean indicates wether or not
--   more results are availble on the server. If that is true, the function should be called again with a higher offset, until (Just _,False) is returned.
--
--   = Example:
--   @
--      dbSearchResults query num =
--          let results = DB.orchestrateCollectionSearchWithOffset query num (num+10)
--          let currentResults = fromJust $ fst results
--          if snd results
--             then currentResults:(dbSearchResults query (num+10))
--             else currentResults
--
--      let dbApplication = DB.createStdApplication \"APPLICATION_NAME\" \"API_KEY\"
--      let dbCollection = DB.createStdCollection \"COLLECTION_NAME\"
--      let completeDBSearchResults = dbSearchResults "QUERY" 0
--   @
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


-- HELPERS

parseListResponseBody :: BSLazy.ByteString -> Maybe [Object]
parseListResponseBody resBodyRaw = do
  result <- decode resBodyRaw
  results <- flip parseMaybe result $ \obj -> obj .: "results" :: Parser [OrchestrateListResult]
  return $ resultValuesAsList results

parseQueryResponseBody :: BSLazy.ByteString -> Maybe ([Object],Integer)
parseQueryResponseBody resBodyRaw = do
  result <- decode resBodyRaw
  (count,results) <- flip parseMaybe result $ \obj -> do
                         count <- obj .: "total_count" :: Parser Integer
                         results <- obj .: "results" :: Parser [OrchestrateQueryResult]
                         return (count,results)
  return (resultValuesAsList results,count)
