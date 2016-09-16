{-# LANGUAGE ScopedTypeVariables,OverloadedStrings #-}

-- simple experiments with the excellent HSparql library

module Main where

import Database.HSparql.Connection  -- for Bound
-- import Database.HSparql.QueryGenerator

import Data.RDF hiding (triple) -- for LNode and PlainLL
-- import Data.RDF.TriplesGraph
-- import Data.Text

import SparqlClient (selectQuery, simpleSelect)
    
main = do
  s <- selectQuery "http://dbpedia.org/sparql" simpleSelect
  case s of
    Just a -> print $ Prelude.map (\[(Bound (LNode (PlainLL s lan)))] -> s) a
    Nothing -> print "nothing"
