{-# LANGUAGE OverloadedStrings #-}

module Main where

import MockupData (synonymsM, tablesL, columnsM)
import Parser -- (parse)
import Data.List.Split
import Data.Char

import Debug.Trace

debug1 query tables columns synonyms =
  trace ("\n* * Processing query: " ++ query) $
    let result = parse query tables columns synonyms in
    mapM_ print result
    
debug2 query tables columns synonyms =
  trace ("\n* * Processing sub syn: " ++ query) $
    print $ substituteSynonyms (splitOn " " (map toLower query)) synonyms

debug3 query tables columns synonyms =
  trace ("\n* * Processing chunk query: " ++ query) $
    let ss = substituteSynonyms (splitOn " " (map toLower query)) synonyms in
    print $ chunkQuery ss
    
main :: IO ()
main = do
  print "Testing parser with mockup data:"
  debug1 "show all products and customers" tablesL columnsM synonymsM
  debug1 "show all customers with more than 5 products" tablesL columnsM synonymsM
  debug2 "show all customers with more than 5 products" tablesL columnsM synonymsM
  debug3 "show all customers with more than 5 products" tablesL columnsM synonymsM
