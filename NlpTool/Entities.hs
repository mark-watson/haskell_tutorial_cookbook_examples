-- Copyright 2014 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of either the GPL version 3 license or the Apache 2 license.

-- Identify entities (people, places, companies, etc.) in text and
-- return entities and URIs to further information in DBPedia/WikiPedia

module Entities (companyNames, peopleNames, countryNames, cityNames, broadcastNetworkNames,
                 politicalPartyNames, tradeUnionNames, universityNames) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (toLower)
import Data.List (sort, intersect, intersperse)
import Data.Set (empty)
import Data.Maybe (isJust)

import Utils (splitWords, bigram, bigram_s, splitWordsKeepCase, trigram, trigram_s, removeDuplicates)

import FirstNames (firstNames)
import LastNames (lastNames)
import NamePrefixes (namePrefixes)

import PeopleDbPedia (peopleMap)

import CountryNamesDbpedia (countryMap)
import CountryNames (countryNamesOneWord, countryNamesTwoWords, countryNamesThreeWords)

import CompanyNamesDbpedia (companyMap)
import CompanyNames (companyNamesOneWord, companyNamesTwoWords, companyNamesThreeWords)
import CityNamesDbpedia (cityMap)
 
import BroadcastNetworkNamesDbPedia (broadcastNetworkMap)
import PoliticalPartyNamesDbPedia (politicalPartyMap)
import TradeUnionNamesDbPedia (tradeUnionMap)
import UniversityNamesDbPedia (universityMap)

isSubsetOf :: (Foldable t1, Foldable t2, Eq a) => t1 a -> t2 a -> Bool
xs `isSubsetOf` ys = all (`elem` ys) xs

namesHelper :: Ord a => [a] -> M.Map a [Char] -> S.Set a -> [(a, Maybe (a, Maybe [Char]))]
namesHelper ngrams dbPediaMap wordMap =
  filter 
    (\x -> case x of
         (_, Just x) -> True
         _ -> False) $
    map (\ngram -> (ngram,
                let v = M.lookup ngram dbPediaMap in
                if isJust v
                   then return (ngram, v)
                   else if S.member ngram wordMap
                           then Just (ngram, Just "")
                           else Nothing)) ngrams   

helperNames1W = namesHelper

helperNames2W wrds = namesHelper (bigram_s wrds)
    
helperNames3W wrds =  namesHelper (trigram_s wrds)

companyNames :: [[Char]] -> [([Char], [Char])]
companyNames wrds =
  let cns = removeDuplicates $ sort $
              helperNames1W wrds companyMap companyNamesOneWord ++
              helperNames2W wrds companyMap companyNamesTwoWords ++
              helperNames3W wrds companyMap companyNamesThreeWords in
  map (\(s, Just (a,Just b)) -> (a,b)) cns
    
countryNames :: [[Char]] -> [([Char], [Char])]
countryNames wrds =
  let cns = removeDuplicates $ sort $
              helperNames1W wrds countryMap countryNamesOneWord ++
              helperNames2W wrds countryMap countryNamesTwoWords ++
              helperNames3W wrds countryMap countryNamesThreeWords in
  map (\(s, Just (a,Just b)) -> (a,b)) cns

entityHelper :: M.Map [Char] [Char] -> [[Char]] -> [([Char], [Char])]
entityHelper entityTypeMap wrds =
  let cns = removeDuplicates $ sort $
              helperNames1W wrds entityTypeMap Data.Set.empty ++
              helperNames2W wrds entityTypeMap Data.Set.empty ++
              helperNames3W wrds entityTypeMap Data.Set.empty in
  map (\(s, Just (a,Just b)) -> (a,b)) cns

peopleNames :: [[Char]] -> [([Char], [Char])]
peopleNames wrds = entityHelper peopleMap wrds

cityNames wrds = entityHelper cityMap wrds

broadcastNetworkNames wrds = entityHelper broadcastNetworkMap wrds

politicalPartyNames wrds = entityHelper politicalPartyMap wrds

tradeUnionNames wrds = entityHelper tradeUnionMap wrds

universityNames wrds = entityHelper universityMap wrds

main = do
    let s = "As read in the San Francisco Chronicle, the company is owned by John Smith, Bill Clinton, Betty Sanders, and Dr. Ben Jones. Ben Jones and Mr. John Smith are childhood friends who grew up in Brazil, Canada, Buenos Aires, and the British Virgin Islands. Apple Computer relased a new version of OS X yesterday. Brazil Brazil Brazil. John Smith bought stock in ConocoPhillips, Heinz, Hasbro, and General Motors, Fox Sports Radio. I listen to B J Cole. Awami National Party is a political party. ALAEA is a trade union. She went to Brandeis University."
    --print $ humanNames s
    print $ peopleNames $ splitWordsKeepCase s
    print $ countryNames $ splitWordsKeepCase s
    print $ companyNames $ splitWordsKeepCase s
    print $ cityNames $ splitWordsKeepCase s
    print $ broadcastNetworkNames $ splitWordsKeepCase s
    print $ politicalPartyNames $ splitWordsKeepCase s
    print $ tradeUnionNames $ splitWordsKeepCase s
    print $ universityNames $ splitWordsKeepCase s
    


    
