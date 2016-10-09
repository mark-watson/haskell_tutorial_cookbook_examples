{-# LANGUAGE OverloadedStrings #-}

module CleanText (cleanText, removeStopWords)  where

import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.Char as C
import Data.List.Utils (replace)

noiseCharacters = ['[', ']', '{', '}', '\n', '\t', '&', '^', 
                   '@', '%', '$', '#', ',']

substituteNoiseCharacters =
  map (\x -> if elem x noiseCharacters then ' ' else x)

cleanText s =
  intercalate
   " " $
   filter
     (\x -> length x > 0) $
     splitOn " " $ substituteNoiseCharacters $
       (replace "." " . "
        (replace "," " , " 
         (replace ";" " ; " s)))

stopWords = ["a", "the", "that", "of", "an", "and"]

toLower' s = map (\x -> if isLower x then x else (C.toLower x)) s

removeStopWords s =
  intercalate
     " " $
    filter
      (\x -> notElem (toLower' x) stopWords) $
      words s

main = do
  let ct = cleanText "The[]@] cat, and all the dogs, escaped&^. They were caught."
  print ct
  let nn = removeStopWords ct
  print nn
