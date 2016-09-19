-- Copyright 2014 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of either the GPL version 3 license or the Apache 2 license.

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

xs `isSubsetOf` ys = all (`elem` ys) xs
    
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

companyNames wrds =
  let cns = removeDuplicates $ sort $
              helperNames1W wrds companyMap companyNamesOneWord ++
              helperNames2W wrds companyMap companyNamesTwoWords ++
              helperNames3W wrds companyMap companyNamesThreeWords in
  map (\(s, Just (a,Just b)) -> (a,b)) cns
  
countryNames wrds =
  let cns = removeDuplicates $ sort $
              helperNames1W wrds countryMap countryNamesOneWord ++
              helperNames2W wrds countryMap countryNamesTwoWords ++
              helperNames3W wrds countryMap countryNamesThreeWords in
  map (\(s, Just (a,Just b)) -> (a,b)) cns

peopleNames wrds =
  let cns = removeDuplicates $ sort $
              helperNames1W wrds peopleMap Data.Set.empty ++
              helperNames2W wrds peopleMap Data.Set.empty ++
              helperNames3W wrds peopleMap Data.Set.empty in
  map (\(s, Just (a,Just b)) -> (a,b)) cns

cityNames wrds =
  let cns = removeDuplicates $ sort $
              helperNames1W wrds cityMap Data.Set.empty ++
              helperNames2W wrds cityMap Data.Set.empty ++
              helperNames3W wrds cityMap Data.Set.empty in
  map (\(s, Just (a,Just b)) -> (a,b)) cns

broadcastNetworkNames wrds =
  let cns = removeDuplicates $ sort $
              helperNames1W wrds broadcastNetworkMap Data.Set.empty ++
              helperNames2W wrds broadcastNetworkMap Data.Set.empty ++
              helperNames3W wrds broadcastNetworkMap Data.Set.empty in
  map (\(s, Just (a,Just b)) -> (a,b)) cns

politicalPartyNames wrds =
  let cns = removeDuplicates $ sort $
              helperNames1W wrds politicalPartyMap Data.Set.empty ++
              helperNames2W wrds politicalPartyMap Data.Set.empty ++
              helperNames3W wrds politicalPartyMap Data.Set.empty in
  map (\(s, Just (a,Just b)) -> (a,b)) cns

tradeUnionNames wrds =
  let cns = removeDuplicates $ sort $
              helperNames1W wrds tradeUnionMap Data.Set.empty ++
              helperNames2W wrds tradeUnionMap Data.Set.empty ++
              helperNames3W wrds tradeUnionMap Data.Set.empty in
  map (\(s, Just (a,Just b)) -> (a,b)) cns

universityNames wrds =
  let cns = removeDuplicates $ sort $
             helperNames1W wrds universityMap Data.Set.empty ++
             helperNames2W wrds universityMap Data.Set.empty ++
             helperNames3W wrds universityMap Data.Set.empty in
  map (\(s, Just (a,Just b)) -> (a,b)) cns


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
    


    
