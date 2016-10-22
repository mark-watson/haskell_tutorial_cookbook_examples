{-# LANGUAGE OverloadedStrings #-}

module HttpSparqlClient where

import Network.HTTP.Conduit (simpleHttp)
import Network.HTTP.Base (urlEncode)
import Text.XML.HXT.Core
import Text.HandsomeSoup
import qualified Data.ByteString.Lazy.Char8 as B

buildQuery :: String -> [Char]
buildQuery sparqlString =
  "http://dbpedia.org/sparql/?query=" ++ urlEncode sparqlString
  
main :: IO ()
main = do
  let query = buildQuery "select * where {<http://dbpedia.org/resource/IBM> <http://dbpedia.org/ontology/abstract> ?o . FILTER langMatches(lang(?o), \"EN\")} LIMIT 100"
  res <- simpleHttp query
  let doc = readString [] (B.unpack res)
  putStrLn "\nAbstracts:\n"
  abstracts <- runX $ doc >>> css "binding" >>> (getAttrValue "name" &&& (deep getText))
  print abstracts
