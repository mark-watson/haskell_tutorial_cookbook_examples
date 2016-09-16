-- Copyright 2014 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of the AGPL version 3 license.

module Summarize (summarize, summarize_s) where

import qualified Data.Map as M
import Data.List.Utils (replace)

import Categorize (bestCategories)
import Sentence (segment)
import Utils (splitWords, bigram_s, cleanText)

import Category1Gram (onegrams)
import Category2Gram (twograms)

scoreSentenceHelper words scoreMap = -- just use 1grams for now
  foldl (+) 0 $ map (\word ->  M.findWithDefault 0.0 word scoreMap) words

safeLookup key alist =
  let x = lookup key alist in
  case x of
    Just v -> v
    Nothing -> 0
 
scoreSentenceByBestCategories words catDataMaps bestCategories =
  map (\(category, aMap) -> 
        (category, (safeLookup category bestCategories) * 
                   (scoreSentenceHelper words aMap))) catDataMaps

scoreForSentence words catDataMaps bestCategories =  
  foldl (+) 0 $ map (\(cat, val) -> val) $ scoreSentenceByBestCategories words catDataMaps bestCategories

summarize s =
  let words = splitWords $ cleanText s;
      bestCats = bestCategories words;
      sentences = segment s;
      result1grams = map (\sentence -> (sentence, scoreForSentence (splitWords sentence) onegrams bestCats)) 
                     sentences;
      result2grams = map (\sentence ->
                           (sentence, scoreForSentence (bigram_s (splitWords sentence)) twograms bestCats)) 
                     sentences; 
      mergedResults = M.toList $ M.unionWith (+) (M.fromList result1grams) (M.fromList result1grams); 
      c400 = filter (\(sentence, score) -> score > 400) mergedResults;
      c300 = filter (\(sentence, score) -> score > 300) mergedResults;
      c200 = filter (\(sentence, score) -> score > 200) mergedResults;
      c100 = filter (\(sentence, score) -> score > 100) mergedResults; 
      c000 = mergedResults in
  if length c400 > 1 then c400 else if length c300 > 1 then c300 else if length c200 > 1 then c200 else if length c100 > 0 then c100 else c000
                          
  
  
summarize_s s =
  let a = replace "\"" "'" $ concat $ map (\x -> (fst x) ++ " ") $ summarize s in
  if length a > 0 then a else safeFirst $ segment s where
    safeFirst x = if length x > 1 then x !! 0 ++ x !! 1 else if length x > 0 then x !! 0 else ""
  
main = do     
  let s = "Plunging European stocks, wobbly bonds and grave concerns about the health of Portuguese lender Banco Espirito Santo SA made last week feel like a rerun of the euro crisis, but most investors say it was no more than a blip for a resurgent region. Banco Espirito Santo has been in investors’ sights since December, when The Wall Street Journal first reported on accounting irregularities at the complex firm. Nerves frayed on Thursday when Banco Espirito Santo's parent company said it wouldn't be able to meet some short-term debt obligations."
  print $ summarize s
  print $ summarize_s s
