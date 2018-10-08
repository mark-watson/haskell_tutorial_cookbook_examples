-- Copyright 2014-2016 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of the GPL version 3 license or Apache 2 license.

module Categorize (bestCategories, splitWords, bigram) where

import qualified Data.Map as M
import Data.List (sortBy)

import Category1Gram (onegrams)
import Category2Gram (twograms)

import Sentence (segment)

import Stemmer (stem)

import Utils (splitWords, bigram, bigram_s)

catnames1 = map fst onegrams
catnames2 = map fst twograms

stemWordsInString s = init $ concatMap ((++ " ") . stem) (splitWords s) -- why discard last item in list?

stemScoredWordList = map (\(str,score) -> (stemWordsInString str, score))

stemHelper = map (\(category, swl) -> (category, M.fromList (stemScoredWordList (M.toList swl))))

stem2 = stemHelper twograms

stem1 = stemHelper onegrams

scoreCat wrds amap =
  sum $ map (\x ->  M.findWithDefault 0.0 x amap) wrds

score wrds amap =
 filter (\(a, b) -> b > 0.9) $ zip [0..] $ map (\(s, m) -> scoreCat wrds m) amap
 
cmpScore (a1, b1) (a2, b2) = compare b2 b1
                              
bestCategoriesHelper wrds ngramMap categoryNames =
  let tg = bigram_s wrds in
    map (\(a, b) -> (categoryNames !! a, b)) $ sortBy cmpScore $ score wrds ngramMap
       
bestCategories1 wrds =
  take 3 $ bestCategoriesHelper wrds onegrams catnames1

bestCategories2 wrds =
  take 3 $ bestCategoriesHelper (bigram_s wrds) twograms catnames2
       
bestCategories1stem wrds =
  take 3 $ bestCategoriesHelper wrds stem1 catnames1

bestCategories2stem wrds =
  take 3 $ bestCategoriesHelper (bigram_s wrds) stem2 catnames2

bestCategories :: [String] -> [(String, Double)]
bestCategories wrds =
  let sum1 = M.unionWith (+) (M.fromList $ bestCategories1 wrds) ( M.fromList $ bestCategories2 wrds)
      sum2 = M.unionWith (+) (M.fromList $ bestCategories1stem wrds) ( M.fromList $ bestCategories2stem wrds) 
  in sortBy cmpScore $ M.toList $ M.unionWith (+) sum1 sum2
      
main = do
    let s = "The sport of hocky is about 100 years old by ahdi dates. American Football is a newer sport. Programming is fun. Congress passed a new budget that might help the economy. The frontier initially was a value path. The ai research of john mccarthy."
    print $ bestCategories1 (splitWords s)    
    print $ bestCategories1stem (splitWords s)
    print $ score (splitWords s) onegrams
    print $ score (bigram_s (splitWords s)) twograms
    print $ bestCategories2 (splitWords s)
    print $ bestCategories2stem (splitWords s)
    print $ bestCategories (splitWords s)
