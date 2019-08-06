{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- this file is just for testing/dev
import Apis (processFilesToNeo4j, processFilesToRdf)
import BlackBoard
import Categorize
import ClassificationWebClient
import CorefWebClient
import DirUtils
import Entities
import FileUtils
import GenNeo4jCypher
import GenTriples
import NlpUtils
import NlpWebClient

import Control.Monad (forM, mapM)
import Data.String.Utils (replace)
import System.Directory (getDirectoryContents)

-- for debug:
import Data.Typeable (typeOf)

import Database.SQLite.Simple

testWebServices :: IO ()
testWebServices
  -- JSON from Coref server (written in Python)
 = do
  s1 <- corefClient "My sister has a dog. She loves him"
  putStrLn $ s1
  s2 <- nlpClient "John Smith went to Mexico."
  putStrLn $ s2

testNlp = do
  let s3 =
        "The sport of hocky is about 100 years old by ahdi dates. American Football is a newer sport. Programming is fun. Congress passed a new budget that might help the economy. The frontier initially was a value path. The ai research of john mccarthy."
  putStrLn "\nbest categories:\n"
  print $ bestCategories (splitWords s3)
  let s4 =
        "As read in the San Francisco Chronicle, the company is owned by John Smith, Bill Clinton, Betty Sanders, and Dr. Ben Jones. Ben Jones and Mr. John Smith are childhood friends who grew up in Brazil, Canada, Buenos Aires, and the British Virgin Islands. Apple Computer relased a new version of OS X yesterday. Brazil Brazil Brazil. John Smith bought stock in ConocoPhillips, Heinz, Hasbro, and General Motors, Fox Sports Radio. I listen to B J Cole. Awami National Party is a political party. ALAEA is a trade union. She went to Brandeis University."
  putStrLn "\nentity tests:\n"
  print $ peopleNames $ splitWordsKeepCase s4
  print $ countryNames $ splitWordsKeepCase s4
  print $ companyNames $ splitWordsKeepCase s4
  print $ cityNames $ splitWordsKeepCase s4
  print $ broadcastNetworkNames $ splitWordsKeepCase s4
  print $ politicalPartyNames $ splitWordsKeepCase s4
  print $ tradeUnionNames $ splitWordsKeepCase s4
  print $ universityNames $ splitWordsKeepCase s4
  putStrLn "\ndirectory iteration test:\n"
  iterateOverDir "." putStrLn
  some_words <- filePathToWordTokens "test_data/test1.txt"
  print some_words
  some_json <- readMetaFile "test_data/test1.meta"
  print $ show some_json
  print $ uri some_json
  putStrLn "\n"
  triples_as_string <-
    textToTriples "test_data/test1.txt" "test_data/test1.meta"
  putStrLn triples_as_string

main :: IO ()
main = do
  putStrLn "KGcreator: files -> RDF triples and Cypher Neo4j data"
  --testWebServices
  --testNlp
  processFilesToRdf "test_data" "out.n3"
  putStrLn "Before processFilesToNeo4j...."
  processFilesToNeo4j "test_data" "out.cypher"
  putStrLn ".... after processFilesToNeo4j"
