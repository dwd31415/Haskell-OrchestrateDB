{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orchestrate.REST
    (
      validateApplication
    ) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Status

import qualified Data.ByteString.Char8 as B
import qualified Control.Exception.Lifted as X

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
