module GenTriples
  ( textToTriples
  , category_to_uri_map
  ) where

import Categorize (bestCategories)
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
import Summarize (summarize, summarizeS)

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

generate_triple :: [Char] -> [Char] -> [Char] -> [Char]
generate_triple s p o = s ++ "  " ++ p ++ "  " ++ o ++ " .\n"

make_literal :: [Char] -> [Char]
make_literal s = "\"" ++ s ++ "\""

category_to_uri_map :: M.Map [Char] [Char]
category_to_uri_map =
  M.fromList
    [ ("news_weather", "<http://knowledgebooks.com/schema/topic/weather>")
    , ("news_war", "<http://knowledgebooks.com/schema/topic/war>")
    , ("economics", "<http://knowledgebooks.com/schema/topic/economics>")
    , ("news_economy", "<http://knowledgebooks.com/schema/topic/economics>")
    , ("news_politics", "<http://knowledgebooks.com/schema/topic/politics>")
    , ("religion", "<http://knowledgebooks.com/schema/topic/religion>")
    , ( "religion_buddhism"
      , "<http://knowledgebooks.com/schema/topic/religion/buddhism>")
    , ( "religion_islam"
      , "<http://knowledgebooks.com/schema/topic/religion/islam>")
    , ( "religion_christianity"
      , "<http://knowledgebooks.com/schema/topic/religion/christianity>")
    , ( "religion_hinduism"
      , "<http://knowledgebooks.com/schema/topic/religion/hinduism>")
    , ( "religion_judaism"
      , "<http://knowledgebooks.com/schema/topic/religion/judaism>")
    , ("chemistry", "<http://knowledgebooks.com/schema/topic/chemistry>")
    , ("computers", "<http://knowledgebooks.com/schema/topic/computers>")
    , ("computers_ai", "<http://knowledgebooks.com/schema/topic/computers/ai>")
    , ( "computers_ai_datamining"
      , "<http://knowledgebooks.com/schema/topic/computers/ai/datamining>")
    , ( "computers_ai_learning"
      , "<http://knowledgebooks.com/schema/topic/computers/ai/learning>")
    , ( "computers_ai_nlp"
      , "<http://knowledgebooks.com/schema/topic/computers/ai/nlp>")
    , ( "computers_ai_search"
      , "<http://knowledgebooks.com/schema/topic/computers/ai/search>")
    , ( "computers_ai_textmining"
      , "<http://knowledgebooks.com/schema/topic/computers/ai/textmining>")
    , ( "computers/programming"
      , "<http://knowledgebooks.com/schema/topic/computers/programming>")
    , ( "computers_microsoft"
      , "<http://knowledgebooks.com/schema/topic/computers/microsoft>")
    , ( "computers/programming/ruby"
      , "<http://knowledgebooks.com/schema/topic/computers/programming/ruby>")
    , ( "computers/programming/lisp"
      , "<http://knowledgebooks.com/schema/topic/computers/programming/lisp>")
    , ("health", "<http://knowledgebooks.com/schema/topic/health>")
    , ( "health_exercise"
      , "<http://knowledgebooks.com/schema/topic/health/exercise>")
    , ( "health_nutrition"
      , "<http://knowledgebooks.com/schema/topic/health/nutrition>")
    , ("mathematics", "<http://knowledgebooks.com/schema/topic/mathematics>")
    , ("news_music", "<http://knowledgebooks.com/schema/topic/music>")
    , ("news_physics", "<http://knowledgebooks.com/schema/topic/physics>")
    , ("news_sports", "<http://knowledgebooks.com/schema/topic/sports>")
    ]

uri_from_category :: [Char] -> [Char]
uri_from_category key =
  fromMaybe ("\"" ++ key ++ "\"") $ M.lookup key category_to_uri_map

textToTriples :: FilePath -> [Char] -> IO [Char]
textToTriples file_path meta_file_path = do
  word_tokens <- filePathToWordTokens file_path
  contents <- filePathToString file_path
  putStrLn $ "** contents:\n" ++ contents ++ "\n"
  meta_data <- readMetaFile meta_file_path
  let people = peopleNames word_tokens
  let companies = companyNames word_tokens
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
  --putStrLn "a_summary:"
  --print a_summary
  --print $ summarize contents

  let summary_triples =
        generate_triple
          (uri meta_data)
          "<http://knowledgebooks.com/schema/summaryOf>" $
        "\"" ++ a_summary ++ "\""
  let category_triples =
        concat
          [ generate_triple
            (uri meta_data)
            "<http://knowledgebooks.com/schema/news/category/>"
            cat
          | cat <- filtered_categories
          ]
  let people_triples1 =
        concat
          [ generate_triple
            (uri meta_data)
            "<http://knowledgebooks.com/schema/containsPersonDbPediaLink>"
            (snd pair)
          | pair <- people
          ]
  let people_triples2 =
        concat
          [ generate_triple
            (snd pair)
            "<http://knowledgebooks.com/schema/aboutPersonName>"
            (make_literal (fst pair))
          | pair <- people
          ]
  let company_triples1 =
        concat
          [ generate_triple
            (uri meta_data)
            "<http://knowledgebooks.com/schema/containsCompanyDbPediaLink>"
            (snd pair)
          | pair <- companies
          ]
  let company_triples2 =
        concat
          [ generate_triple
            (snd pair)
            "<http://knowledgebooks.com/schema/aboutCompanyName>"
            (make_literal (fst pair))
          | pair <- companies
          ]
  let country_triples1 =
        concat
          [ generate_triple
            (uri meta_data)
            "<http://knowledgebooks.com/schema/containsCountryDbPediaLink>"
            (snd pair)
          | pair <- countries
          ]
  let country_triples2 =
        concat
          [ generate_triple
            (snd pair)
            "<http://knowledgebooks.com/schema/aboutCountryName>"
            (make_literal (fst pair))
          | pair <- countries
          ]
  let city_triples1 =
        concat
          [ generate_triple
            (uri meta_data)
            "<http://knowledgebooks.com/schema/containsCityDbPediaLink>"
            (snd pair)
          | pair <- cities
          ]
  let city_triples2 =
        concat
          [ generate_triple
            (snd pair)
            "<http://knowledgebooks.com/schema/aboutCityName>"
            (make_literal (fst pair))
          | pair <- cities
          ]
  let bnetworks_triples1 =
        concat
          [ generate_triple
            (uri meta_data)
            "<http://knowledgebooks.com/schema/containsBroadCastDbPediaLink>"
            (snd pair)
          | pair <- broadcast_networks
          ]
  let bnetworks_triples2 =
        concat
          [ generate_triple
            (snd pair)
            "<http://knowledgebooks.com/schema/aboutBroadCastName>"
            (make_literal (fst pair))
          | pair <- broadcast_networks
          ]
  let pparties_triples1 =
        concat
          [ generate_triple
            (uri meta_data)
            "<http://knowledgebooks.com/schema/containsPoliticalPartyDbPediaLink>"
            (snd pair)
          | pair <- political_parties
          ]
  let pparties_triples2 =
        concat
          [ generate_triple
            (snd pair)
            "<http://knowledgebooks.com/schema/aboutPoliticalPartyName>"
            (make_literal (fst pair))
          | pair <- political_parties
          ]
  let unions_triples1 =
        concat
          [ generate_triple
            (uri meta_data)
            "<http://knowledgebooks.com/schema/containsTradeUnionDbPediaLink>"
            (snd pair)
          | pair <- trade_unions
          ]
  let unions_triples2 =
        concat
          [ generate_triple
            (snd pair)
            "<http://knowledgebooks.com/schema/aboutTradeUnionName>"
            (make_literal (fst pair))
          | pair <- trade_unions
          ]
  let universities_triples1 =
        concat
          [ generate_triple
            (uri meta_data)
            "<http://knowledgebooks.com/schema/containsUniversityDbPediaLink>"
            (snd pair)
          | pair <- universities
          ]
  let universities_triples2 =
        concat
          [ generate_triple
            (snd pair)
            "<http://knowledgebooks.com/schema/aboutTradeUnionName>"
            (make_literal (fst pair))
          | pair <- universities
          ]
  return $
    concat
      [ people_triples1
      , people_triples2
      , company_triples1
      , company_triples2
      , country_triples1
      , country_triples2
      , city_triples1
      , city_triples2
      , bnetworks_triples1
      , bnetworks_triples2
      , pparties_triples1
      , pparties_triples2
      , unions_triples1
      , unions_triples2
      , universities_triples1
      , universities_triples2
      , category_triples
      , summary_triples
      ]
