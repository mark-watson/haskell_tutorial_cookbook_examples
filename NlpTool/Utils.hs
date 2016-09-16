-- Copyright 2014 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of the GPL version 3 license.

module Utils (splitWords, bigram, bigram_s, splitWordsKeepCase, trigram, trigram_s, removeDuplicates, cleanText) where

import Data.Char (toLower)
import Data.Set (toList, fromList)
import Data.String.Utils (replace)

cleanText s = replace "“" "\"" $ replace "”" "\"" $ replace "’" "'" s

splitWords :: String -> [String]
splitWords = words . map (\c -> if elem c ".,;:!\n\t\"" then ' ' else toLower c)

bigram :: [a] -> [[a]]
bigram [] = []
bigram [_] = []
bigram xs = take 2 xs : bigram (tail xs)

bigram_s xs = [ (a !! 0) ++ " " ++ (a !! 1) | a <- bigram xs] 

splitWordsKeepCase :: String -> [String]
splitWordsKeepCase = words . map (\c -> if elem c ".,;:!\n\t\"" then ' ' else c)

trigram :: [a] -> [[a]]
trigram [] = []
trigram [_] = []
trigram [_,_] = []
trigram xs = take 3 xs : trigram (tail xs)

trigram_s xs = [ (a !! 0) ++ " " ++ (a !! 1) ++ " " ++ (a !! 2) | a <- trigram xs] 

 
removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = toList . fromList
