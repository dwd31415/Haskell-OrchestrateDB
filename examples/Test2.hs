 {-# LANGUAGE DeriveGeneric      #-}
module Main where

import GHC.Generics
import Data.Aeson
import qualified Database.Orchestrate as DB

data SherlockHolmesCase = SherlockHolmesCase {
  title :: String,
  typeOfCrime :: String,
  shortStory :: Bool,
  index :: Integer,
  solved :: Bool
} deriving (Show,Read,Generic,Eq)

 {-This automatically creates both a JSON parser and a JSON encoder based on the data type SherlockHolmesCase, if you however
want the property names in JSON and haskell to differ, you will have to write your own parser and you own encoder.
 Please check out the 'Data.Aeson' documentation for information on how to do that. -}
instance FromJSON SherlockHolmesCase
instance ToJSON SherlockHolmesCase

main :: IO ()
main = do
  configTxt <- readFile "examples/config.txt"
  let config = read configTxt :: (String,String)
  -- Create an application record. Oh, BTW, in case you planned to store your api-key in the source code, it is a very very bad idea.
  let dbApplcation = DB.createStdApplication  "your.orchestrate.application.name "  (fst config)
  -- Create an collection record. This collection does not have to exist, if it does not, it will be created as soon as you try to store something in it.
  let dbCollection = DB.createStdCollection  "TheCasebookOfSherlockHolmes "
  let aStudyInScarlet = SherlockHolmesCase {
    title =  "A Study in Scarlet ",
    typeOfCrime =  "Murder ",
    shortStory = False,
    index = 0,
    solved = True
  }
  -- Store a haskell value in the collection we created earlier. The  "withOutKey " lets the server generate the key, so no key must be specified.
  didItWork <- DB.orchestrateCollectionPutWithoutKey dbApplcation dbCollection aStudyInScarlet
  if didItWork
    then putStrLn "Invictus maneo!"
    else putStrLn "Something is rotten in the state of Denmark!"
