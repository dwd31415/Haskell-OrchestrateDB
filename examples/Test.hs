{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Main where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           GHC.Generics
import qualified Orchestrate      as DB
import           System.Exit      (exitFailure)
import           System.Random

data TestRecord = TestRecord
    { string :: String
    , number :: Int
    } deriving (Show,Read,Generic,Eq)

instance FromJSON TestRecord
instance ToJSON TestRecord

reachTest :: Integer -> IO ()
reachTest number = do
  putStrLn $ "Completed test number " ++ show number

main :: IO ()
main = do
  configTxt <- readFile "examples/config.txt"
  randomNumber <- randomIO :: IO Integer
  let config = read configTxt :: (String,String)
  let dbApplication = DB.createStdApplication "TestApp" (fst config)
  let dbCollection = DB.createStdCollection (snd config)
  let testRecord1 = TestRecord {string = "You may delay, but time will not.(Benjamin Franklin)",number = 903}
  let testRecord2 = TestRecord {string = "Two things are infinite: the universe and human stupidity; and I am not sure about the universe.(Albert Einstein)",number = 913}
  res1 <- DB.validateApplication dbApplication
  unless res1 exitFailure
  reachTest 1
  let testKey = "hello-world" ++ show randomNumber
  res2 <- DB.orchestrateCollectionPut dbApplication dbCollection testKey testRecord1
  unless res2 exitFailure
  reachTest 2
  res3 <- DB.orchestrateCollectionPut dbApplication dbCollection "irrelevant" testRecord1
  unless res3 exitFailure
  reachTest 3
  res4 <- DB.orchestrateCollectionPutWithoutKey dbApplication dbCollection testRecord2
  unless res4 exitFailure
  reachTest 4
  shouldBeTestRecord1Maybe <- DB.orchestrateCollectionGet dbApplication dbCollection testKey :: IO (Maybe TestRecord)
  let shouldBeTestRecord1 = fromJust shouldBeTestRecord1Maybe
  unless (shouldBeTestRecord1 == testRecord1) exitFailure
  reachTest 5
  res5 <- DB.orchestrateCollectionDeleteKey dbApplication dbCollection "irrelevant"
  unless res5 exitFailure
  reachTest 6
  res6Maybe <- DB.orchestrateCollectionSearch dbApplication dbCollection "You may delay,"
  let (res6,hasNextPage) = fromJust res6Maybe
  unless (checkIfResSixIsCorrect res6) exitFailure
  reachTest 7
  res7Maybe <- DB.orchestrateCollectionList dbApplication dbCollection 100
  let res7 = fromJust res7Maybe
  unless (res7 /= []) exitFailure
  reachTest 8

checkIfResSixIsCorrect :: [Object] -> Bool
checkIfResSixIsCorrect (first:rest) = do
  let num = flip parseMaybe first $ \obj -> obj .: "number" :: Parser Int
  isJust num &&
    (if fromJust num == 903 then checkIfResSixIsCorrect rest else
       False)

checkIfResSixIsCorrect [] = True
