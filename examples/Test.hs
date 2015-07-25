{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import System.Exit (exitFailure)
import Data.Aeson
import GHC.Generics
import Control.Monad
import Data.Maybe
import qualified Orchestrate as DB

data TestRecord = TestRecord
    { string :: String
    , number  :: Int
    } deriving (Show,Read,Generic,Eq)

instance FromJSON TestRecord
instance ToJSON TestRecord

reachTest :: Integer -> IO ()
reachTest number = do
  putStrLn $ "Completed test number " ++ show number

main :: IO ()
main = do
  configTxt <- readFile "examples/config.txt"
  let config = read configTxt :: (String,String)
  let dbApplication = DB.createStdApplication "TestApp" (fst config)
  let dbCollection = DB.createStdCollection (snd config)
  let testRecord1 = TestRecord {string = "You may delay, but time will not!",number = 903}
  let testRecord2 = TestRecord {string = "You may delay, but time will not!",number = 913}
  res1 <- DB.validateApplication dbApplication
  unless res1 exitFailure
  reachTest 1
  res2 <- DB.orchestrateCollectionPut dbApplication dbCollection "hello-world" testRecord1
  unless res2 exitFailure
  reachTest 2
  res3 <- DB.orchestrateCollectionPutWithoutKey dbApplication dbCollection testRecord2
  unless res3 exitFailure
  reachTest 3
  shouldBeTestRecord1Maybe <- DB.orchestrateCollectionGet dbApplication dbCollection "hello-world" :: IO (Maybe TestRecord)
  let shouldBeTestRecord1 = fromJust shouldBeTestRecord1Maybe
  unless (shouldBeTestRecord1 == testRecord1) exitFailure
  reachTest 4