{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Text.JSON.Generic

--                 DOES NOT WORK, just experimenting:

import Network.HTTP.Conduit (simpleHttp)
import Network.HTTP.Base (urlEncode)
--import Text.XML.HXT.Core
import Text.HandsomeSoup
import qualified Data.ByteString.Lazy.Char8 as B
--import qualified Data.Text as T

prefixUrl :: [Char]
prefixUrl = "http://dbpedia.org/sparql/?query="

buildQuery :: String -> [Char]
buildQuery sparqlString = prefixUrl ++ urlEncode sparqlString ++ "&format=json"
  
main :: IO ()
main = do
  let query = buildQuery "select * where {<http://dbpedia.org/resource/IBM> <http://dbpedia.org/ontology/abstract> ?o . FILTER langMatches(lang(?o), \"EN\")} LIMIT 100"
  res <- simpleHttp query
  --let doc = readJSString (B.unpack res)
  --putStrLn (show doc) 
  putStrLn "\nAbstracts:\n"
  -- abstracts <- runX $ doc >>> css "binding" >>> (getAttrValue "name" &&& (deep getText))
  -- print abstracts
