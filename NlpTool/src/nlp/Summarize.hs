-- Copyright 2014 by Mark Watson. All rights reserved.
-- The software and data in this project can be used under the terms of the AGPL version 3 license or Apache 2 license.
module Summarize
  ( summarize
  , summarizeS
  ) where

import Categorize (bestCategories)
import Data.List.Utils (replace)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import NlpUtils (bigram_s, cleanText, splitWords)
import Sentence (segment)

import Category1Gram (onegrams)
import Category2Gram (twograms)

scoreSentenceHelper words scoreMap -- just use 1grams for now
 = sum $ map (\word -> M.findWithDefault 0.0 word scoreMap) words

safeLookup key alist = fromMaybe 0 $ lookup key alist

scoreSentenceByBestCategories words catDataMaps bestCategories =
  map
    (\(category, aMap) ->
       ( category
       , safeLookup category bestCategories * scoreSentenceHelper words aMap))
    catDataMaps

scoreForSentence words catDataMaps bestCategories =
  sum $ map snd $ scoreSentenceByBestCategories words catDataMaps bestCategories

summarize s =
  let words = splitWords $ cleanText s
      bestCats = bestCategories words
      sentences = segment s
      result1grams =
        map
          (\sentence ->
             ( sentence
             , scoreForSentence (splitWords sentence) onegrams bestCats))
          sentences
      result2grams =
        map
          (\sentence ->
             ( sentence
             , scoreForSentence
                 (bigram_s (splitWords sentence))
                 twograms
                 bestCats))
          sentences
      mergedResults =
        filter (\(s, v) -> v > 0.05) $
        M.toList $
        M.unionWith (+) (M.fromList result1grams) (M.fromList result1grams)
      c400 = filter (\(sentence, score) -> score > 10) mergedResults
      c300 = filter (\(sentence, score) -> score > 7) mergedResults
      c280 = filter (\(sentence, score) -> score > 4) mergedResults
      c250 = filter (\(sentence, score) -> score > 3) mergedResults
      c200 = filter (\(sentence, score) -> score > 2) mergedResults
      c100 = filter (\(sentence, score) -> score > 1) mergedResults
      c050 = filter (\(sentence, score) -> score > 0.5) mergedResults
      c000 = mergedResults
   in if (not (null c400)) && (length c400) > 1 && (length c400) < 3
        then c400
        else if (not (null c300)) && (length c300) > 1 && (length c300) < 3
               then c300
               else if not (null c280) && (length c280) > 1 && (length c280) < 3
                      then c280
                      else if not (null c250) &&
                              (length c250) > 1 && (length c250) < 3
                             then c250
                             else if not (null c200) &&
                                     (length c200) > 1 && (length c200) < 3
                                    then c200
                                    else if not (null c100) &&
                                            (length c100) > 1 &&
                                            (length c100) < 3
                                           then c100
                                           else if not (null c050) &&
                                                   (length c050) > 1 &&
                                                   (length c050) < 3
                                                  then c050
                                                  else if (length mergedResults) <
                                                          3
                                                         then c000
                                                         else [ head
                                                                  mergedResults
                                                              , (head . tail)
                                                                  mergedResults
                                                              ]

summarizeS s =
  let a =
        replace "\"" "'" $
        replace "\n" " " $ concatMap (\x -> fst x ++ " ") $ summarize s
   in init $
      if not (null a)
        then a
        else safeFirst $ segment s
  where
    safeFirst x
      | length x > 1 = head x ++ x !! 1
      | not (null x) = head x
      | otherwise = " "

main = do
  let s =
        "Plunging European stocks, wobbly bonds and grave concerns about the health of Portuguese lender Banco Espirito Santo SA made last week feel like a rerun of the euro crisis, but most investors say it was no more than a blip for a resurgent region. Banco Espirito Santo has been in investors’ sights since December, when The Wall Street Journal first reported on accounting irregularities at the complex firm. Nerves frayed on Thursday when Banco Espirito Santo's parent company said it wouldn't be able to meet some short-term debt obligations."
  print $ summarize s
  print $ summarizeS s
