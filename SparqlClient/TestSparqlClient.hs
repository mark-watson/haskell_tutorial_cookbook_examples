-- simple experiments with the excellent HSparql library

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.HSparql.Connection (BindingValue(Bound))

import Data.RDF hiding (triple)
import Database.HSparql.QueryGenerator
import Database.HSparql.Connection (selectQuery)
    
webBrowserSelect :: Query SelectQuery
webBrowserSelect = do
    resource <- prefix "dbprop" (iriRef "http://dbpedia.org/resource/")
    dbpprop  <- prefix "dbpedia" (iriRef "http://dbpedia.org/property/")
    foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
    x    <- var
    name <- var
    triple x (dbpprop .:. "genre") (resource .:. "Web_browser")
    triple x (foaf .:. "name") name

    return SelectQuery { queryVars = [name] }

companyAbstractSelect :: Query SelectQuery
companyAbstractSelect = do
    resource <- prefix "dbprop" (iriRef "http://dbpedia.org/resource/")
    ontology <- prefix "ontology" (iriRef "http://dbpedia.org/ontology/")
    o <- var
    triple (resource .:. "Edinburgh_University_Press") (ontology .:. "abstract") o
    return SelectQuery { queryVars = [o] }

companyTypeSelect :: Query SelectQuery
companyTypeSelect = do
    resource <- prefix "dbprop" (iriRef "http://dbpedia.org/resource/")
    ontology <- prefix "ontology" (iriRef "http://dbpedia.org/ontology/")
    o <- var
    triple (resource .:. "Edinburgh_University_Press") (ontology .:. "type") o
    return SelectQuery { queryVars = [o] }

main :: IO ()
main = do
  sq1 <- selectQuery "http://dbpedia.org/sparql" companyAbstractSelect
  --putStrLn "\nRaw results of company abstract SPARQL query:\n"
  --print sq1
  putStrLn "\nWeb browser names extracted from the company abstract query results:\n"
  case sq1 of
    Just a -> print $ map (\[Bound (LNode (PlainLL s _))] -> s) a
    Nothing -> putStrLn "nothing"
  sq2 <- selectQuery "http://dbpedia.org/sparql" companyTypeSelect
  --putStrLn "\nRaw results of company type SPARQL query:\n"
  --print sq2
  putStrLn "\nWeb browser names extracted from the company type query results:\n"
  case sq2 of
    Just a -> print $ map (\[Bound (UNode  s)] -> s) a
    Nothing -> putStrLn "nothing"
  sq3 <- selectQuery "http://dbpedia.org/sparql" webBrowserSelect
  --putStrLn "\nRaw results of SPARQL query:\n"
  --print sq3
  putStrLn "\nWeb browser names extracted from the query results:\n"
  case sq3 of
    Just a -> print $ map (\[Bound (LNode (PlainLL s _))] -> s) a
    Nothing -> putStrLn "nothing"
