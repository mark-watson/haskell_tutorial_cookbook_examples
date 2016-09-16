{-# LANGUAGE OverloadedStrings #-}

module Main where

import MockupData (synonymsM, tablesL, columnsM)
import Parser (parse)

import Debug.Trace

debug query tables columns synonyms =
  trace ("\n* * Processing query: " ++ query) $
    let result = parse query tables columns synonyms in
    mapM_ print result
    
main :: IO ()
main = do
  print "Testing parser with mockup data:"
  debug "show all products and customers" tablesL columnsM synonymsM
  debug "show all customers with more than 5 products" tablesL columnsM synonymsM

