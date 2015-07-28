# Haskell-OrchestrateDB [![Build Status](https://travis-ci.org/dwd31415/Haskell-OrchestrateDB.svg?branch=master)](https://travis-ci.org/dwd31415/Haskell-OrchestrateDB)
Unofficial Haskell Client Library for the Orchestrate.io API


This library implements most parts of the Orchestrate.io API in Haskell. It provides a convenient way of accessing these parts of the REST API:

  *  	Validate API Keys

  *   List Key/Values

  *   Create Key/Value

  *   Create Key/Value with server-generated key

  *   Update Key/Value

  *   Retrieve Value for Key

  *  	Delete Collection(s)

  *   Query Collection

NOTE: You can also use 'Network.Curl', if you need one of the currently unsupported API functions, but you would have to parse all response bodies yourself if you do that.

=How to use this library?
Using this library is pretty straightforward, just import the Orchestrate module like this:

@
import qualified Orchestrate as DB
@

Once you have it, you can use all the supported API functions(Check out the documentation on 'Orchestrate.REST' for detailed information).
One important thing you need to remeber when working with this library is, that it uses 'Data.Aeson' under the hood, so if you want to
store your Haskell types in an Orchestrate database, 'Data.Aseon' must know how convert them into JSON. The best way to achive that, is
to use the 'DeriveGeneric' Langauge extension.

Here is an example of how to store a haskell value in an Orchestrate Collection:

@
 {-# LANGUAGE DeriveGeneric      #-}
module Main where

import GHC.Generics
import Data.Aeson
import qualified Orchestrate as DB

data SherlockHolmesCase = SherlockHolmesCase {
  title :: String,
  typeOfCrime :: String,
  shortStory :: Bool,
  index :: Integer,
  solved :: Bool
} deriving (Show,Read,Generic,Eq)

 {- This automatically creates both a JSON parser and a JSON encoder based on the data type SherlockHolmesCase, if you however
want the property names in JSON and haskell to differ, you will have to write your own parser and you own encoder.
 Please check out the 'Data.Aeson' documentation for information on how to do that. -}
instance FromJSON SherlockHolmesCase
instance ToJSON SherlockHolmesCase

main :: IO ()
main = do
  -- Create an application record. Oh, BTW, in case you planned to store your api-key in the source code, it is a very very bad idea.
  let dbApplcation = DB.createStdApplication  "your.orchestrate.application.name "  "one-of-your-api-keys "
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
@

If this application does say  "Invictus maneo! ", you should get this result if you LIST the  "TheCasebookOfSherlockHolmes " collection:

@
"count" : 1,
  "results" : [ {
    "path" : {
      "collection" : "TheCasebookOfSherlockHolmes ",
      "kind" : "item",
      "key" : "0b8b506cf0204ce9",
      "ref" : "--------------",
      "reftime" : --------------
    },
    "value" : {
      "solved" : true,
      "shortStory" : false,
      "title" : "A Study in Scarlet ",
      "index" : 0,
      "typeOfCrime" : "Murder "
    },
    "reftime" : --------------
  } ]
@

If you don't get that result, you either have run the example more than once, it did not return "Invictus maneo", or something really strange is going on.
If you are fairly certain, that something strange is going on, and that it is my fault, you might want to open an issue at github.

# How to use the "cabal test" command
Testing applications which depend on some REST api is not easy, and therfore the cabal tests will fail by default. They just will not be able to authenticate, if you
want to run the tests on your local machine, please create a file called "examples/config.txt" and fill it according to this scheme:

@
("a valid api key","something funny(it will be used as a name for the test collection)")
@

If you are asking yourself now, if there really is no way of checking that this library works, before installing it, there is nothing to work about.
You should consult travis-ci<https://travis-ci.org/dwd31415/Haskell-OrchestrateDB> on the state of the project, it does not only check wether or not ther library can be built, but also runs the tests.
