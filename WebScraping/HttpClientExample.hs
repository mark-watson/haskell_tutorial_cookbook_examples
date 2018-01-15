{-# LANGUAGE OverloadedStrings #-}

-- reference: http://www.serpentine.com/wreq/tutorial.html

module Main where

import Network.Wreq
import Control.Lens  -- for ^. and ^?
import Data.Maybe (fromJust)

fetchURI uri = do
  putStrLn $ "\n\n***  Fetching " ++ uri
  r <- get uri
  putStrLn $ "status code: " ++ (show (r ^. responseStatus . statusCode))
  putStrLn $ "content type: " ++ (show (r ^? responseHeader "Content-Type"))
  putStrLn $ "respose body: " ++ show (fromJust (r ^? responseBody))
  
main :: IO ()
main = do
  -- JSON from DBPedia
  fetchURI "http://dbpedia.org/data/Sedona_Arizona.json"
  -- N3 RDF from DBPedia
  fetchURI "http://dbpedia.org/data/Sedona_Arizona.n3"
  -- HTML from my web site
  fetchURI "http://markwatson.com"

