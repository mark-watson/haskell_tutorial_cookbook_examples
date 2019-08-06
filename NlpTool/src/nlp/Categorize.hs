-- Copyright 2014-2016 by Mark Watson. All rights reserved.
-- The software and data in this project can be used under the terms of the AGPL version 3.
module Categorize
  ( bestCategories
  , splitWords
  , bigram
  ) where

import Data.List (sortBy)
import qualified Data.Map as M

import Category1Gram (onegrams)
import Category2Gram (twograms)

import Sentence (segment)

import Stemmer (stem)

import NlpUtils (bigram, bigram_s, splitWords)

catnames1 = map fst onegrams

catnames2 = map fst twograms

stemWordsInString :: String -> [Char]
stemWordsInString s = init $ concatMap ((++ " ") . stem) (splitWords s) -- why discard last item in list?

stemScoredWordList = map (\(str, score) -> (stemWordsInString str, score))

stemHelper =
  map
    (\(category, swl) ->
       (category, M.fromList (stemScoredWordList (M.toList swl))))

stem2 :: [([Char], M.Map [Char] Double)]
stem2 = stemHelper twograms

stem1 :: [([Char], M.Map [Char] Double)]
stem1 = stemHelper onegrams

scoreCat wrds amap = sum $ map (\x -> M.findWithDefault 0.0 x amap) wrds

score wrds amap =
  filter (\(a, b) -> b > 0.9) $
  zip [0 ..] $ map (\(s, m) -> scoreCat wrds m) amap

cmpScore (a1, b1) (a2, b2) = compare b2 b1

bestCategoriesHelper wrds ngramMap categoryNames =
  let tg = bigram_s wrds
   in map (\(a, b) -> (categoryNames !! a, b)) $
      sortBy cmpScore $ score wrds ngramMap

bestCategories1 wrds = take 3 $ bestCategoriesHelper wrds onegrams catnames1

bestCategories2 :: [[Char]] -> [([Char], Double)]
bestCategories2 wrds =
  take 3 $ bestCategoriesHelper (bigram_s wrds) twograms catnames2

bestCategories1stem :: [[Char]] -> [([Char], Double)]
bestCategories1stem wrds = take 3 $ bestCategoriesHelper wrds stem1 catnames1

bestCategories2stem :: [[Char]] -> [([Char], Double)]
bestCategories2stem wrds =
  take 3 $ bestCategoriesHelper (bigram_s wrds) stem2 catnames2

bestCategories :: [String] -> [(String, Double)]
bestCategories wrds = map do_normalization_to_probabilities non_normalized
  where
    sum1 =
      M.unionWith
        (+)
        (M.fromList $ bestCategories1 wrds)
        (M.fromList $ bestCategories2 wrds)
    sum2 =
      M.unionWith
        (+)
        (M.fromList $ bestCategories1stem wrds)
        (M.fromList $ bestCategories2stem wrds)
    non_normalized = sortBy cmpScore $ M.toList $ M.unionWith (+) sum1 sum2
    total_scores = foldl (+) 0 $ map snd non_normalized
    do_normalization_to_probabilities (name, value) =
      (name, value / total_scores)
