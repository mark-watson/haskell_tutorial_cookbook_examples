{-# LANGUAGE OverloadedStrings #-}

module GenNeo4jCypher
  ( textToCypher
  , neo4j_category_node_defs
  ) where

import Categorize (bestCategories)
import Data.List (isInfixOf)
import Data.Char (toLower)
import Data.String.Utils (replace)
import Entities
  ( broadcastNetworkNames
  , cityNames
  , companyNames
  , countryNames
  , peopleNames
  , politicalPartyNames
  , tradeUnionNames
  , universityNames
  )
import FileUtils
  ( MyMeta
  , filePathToString
  , filePathToWordTokens
  , readMetaFile
  , uri
  )
import GenTriples (category_to_uri_map)
import Summarize (summarize, summarizeS)

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

neo4j_category_node_defs :: [Char]
neo4j_category_node_defs =
  replace
    "/"
    "_"
    $ concat
    [ "CREATE (" ++ c ++ ":CategoryType {name:\"" ++ c ++ "\"})\n"
    | c <- M.keys category_to_uri_map
    ]

uri_from_category :: p -> p
uri_from_category s = s -- might want the full version from GenTriples

repl :: Char -> Char
repl '-' = '_'
repl '/' = '_'
repl '.' = '_'
repl c = c

filterChars :: [Char] -> [Char]
filterChars = filter (\c -> c /= '?' && c /= '=' && c /= '<' && c /= '>')

create_neo4j_node :: [Char] -> ([Char], [Char])
create_neo4j_node uri =
  let name =
        (map repl (filterChars
                    (replace "https://" "" (replace "http://" "" uri)))) ++
                    "_" ++
                    (map toLower node_type)
      node_type =
        if isInfixOf "dbpedia" uri
          then "DbPedia"
          else "News"
      new_node =
        "CREATE (" ++
        name ++ ":" ++
        node_type ++ " {name:\"" ++ (replace " " "_" name) ++
        "\", uri:\"" ++ uri ++ "\"})\n"
   in (name, new_node)

create_neo4j_link :: [Char] -> [Char] -> [Char] -> [Char]
create_neo4j_link node1 linkName node2 =
  "CREATE (" ++ node1 ++ ")-[:" ++ linkName ++ "]->(" ++ node2 ++ ")\n"

create_summary_node :: [Char] -> [Char] -> [Char]
create_summary_node uri summary =
  let name =
        "summary_of_" ++
        (map repl $
         filterChars (replace "https://" "" (replace "http://" "" uri)))
      s1 = "CREATE (" ++ name ++ ":Summary {name:\"" ++ name ++ "\", uri:\""
      s2 = uri ++ "\", summary:\"" ++ summary ++ "\"})\n"
   in s1 ++ s2

create_entity_node :: ([Char], [Char]) -> [Char]
create_entity_node entity_pair = 
  "CREATE (" ++ (replace " " "_" (fst entity_pair)) ++ 
  ":Entity {name:\"" ++ (fst entity_pair) ++ "\", uri:\"" ++
  (snd entity_pair) ++ "\"})\n"

create_contains_entity :: [Char] -> [Char] -> ([Char], [Char]) -> [Char]
create_contains_entity relation_name source_uri entity_pair =
  let new_person_node = create_entity_node entity_pair
      new_link = create_neo4j_link source_uri
                   relation_name
                   (replace " " "_" (fst entity_pair))
  in
    (new_person_node ++ new_link)

entity_node_helper :: [Char] -> [Char] -> [([Char], [Char])] -> [Char]
entity_node_helper relation_name node_name entity_list =
  concat [create_contains_entity
           relation_name node_name entity | entity <- entity_list]

textToCypher :: FilePath -> [Char] -> IO [Char]
textToCypher file_path meta_file_path = do
  let prelude_nodes = neo4j_category_node_defs
  putStrLn "+++++++++++++++++ prelude node defs:"
  print prelude_nodes
  word_tokens <- filePathToWordTokens file_path
  contents <- filePathToString file_path
  putStrLn $ "** contents:\n" ++ contents ++ "\n"
  meta_data <- readMetaFile meta_file_path
  putStrLn "++ meta_data:"
  print meta_data
  let people = peopleNames word_tokens
  let companies = companyNames word_tokens
  putStrLn "^^^^ companies:"
  print companies
  let countries = countryNames word_tokens
  let cities = cityNames word_tokens
  let broadcast_networks = broadcastNetworkNames word_tokens
  let political_parties = politicalPartyNames word_tokens
  let trade_unions = tradeUnionNames word_tokens
  let universities = universityNames word_tokens
  let a_summary = summarizeS contents
  let the_categories = bestCategories word_tokens
  let filtered_categories =
        map (uri_from_category . fst) $
        filter (\(name, value) -> value > 0.3) the_categories
  putStrLn "\nfiltered_categories:"
  print filtered_categories
  let (node1_name, node1) = create_neo4j_node (uri meta_data)
  let summary1 = create_summary_node (uri meta_data) a_summary
  let category1 =
        concat
          [ create_neo4j_link node1_name "Category" cat
          | cat <- filtered_categories
          ]
  let pp = entity_node_helper "ContainsPersonDbPediaLink" node1_name people
  let cmpny = entity_node_helper "ContainsCompanyDbPediaLink" node1_name companies
  let cntry = entity_node_helper "ContainsCountryDbPediaLink" node1_name countries
  let citys = entity_node_helper "ContainsCityDbPediaLink" node1_name cities
  let bnet = entity_node_helper "ContainsBroadcastNetworkDbPediaLink"
                                node1_name broadcast_networks
  let ppart = entity_node_helper "ContainsPoliticalPartyDbPediaLink"
                                node1_name political_parties
  let tunion = entity_node_helper "ContainsTradeUnionDbPediaLink"
                                  node1_name trade_unions
  let uni = entity_node_helper "ContainsUniversityDbPediaLink"
                               node1_name universities
  return $ concat [node1, summary1, category1, pp, cmpny, cntry, citys, bnet,
                   ppart, tunion, uni]
