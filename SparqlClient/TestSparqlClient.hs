--{-# LANGUAGE ScopedTypeVariables,OverloadedStrings #-}

-- simple experiments with the excellent HSparql library

module Main where

import Database.HSparql.Connection (BindingValue(Bound))

import Data.RDF hiding (triple)
import Database.HSparql.QueryGenerator
import Database.HSparql.Connection (selectQuery)
    
simpleSelect :: Query SelectQuery
simpleSelect = do
    resource <- prefix "dbprop" (iriRef "http://dbpedia.org/resource/")
    dbpprop  <- prefix "dbpedia" (iriRef "http://dbpedia.org/property/")
    foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
    x    <- var
    name <- var
    triple x (dbpprop .:. "genre") (resource .:. "Web_browser")
    triple x (foaf .:. "name") name

    return SelectQuery { queryVars = [name] }

main :: IO ()
main = do
  sq <- selectQuery "http://dbpedia.org/sparql" simpleSelect
  putStrLn "\nRaw results of SPARQL query:\n"
  print sq
  putStrLn "\nWeb browser names extracted from the query results:\n"
  case sq of
    Just a -> print $ map (\[Bound (LNode (PlainLL s _))] -> s) a
    Nothing -> putStrLn "nothing"
