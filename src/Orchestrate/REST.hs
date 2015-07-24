{-# LANGUAGE OverloadedStrings #-}

module Orchestrate.REST
    (
      validateApplication
    ) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Status

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
        Just request -> withManager $ \manager -> do
                    let reqHead = request {
                                            method = "HEAD",
                                            secure = True }
                    res <- http reqHead manager
                    if responseStatus res == ok200
                      then return True
                      else return False
