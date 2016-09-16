module Main where

import Data.Maybe
import System.Random -- uses random library (see Pure.cabal file)

spaceship n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1
              
randomMaybeValue n
  | n `mod` 2 == 0 = Just n
  | otherwise = Nothing
    
main = do
  print $ spaceship (-100)
  print $ spaceship 0
  print $ spaceship 17
  print $ randomMaybeValue 1
  print $ randomMaybeValue 2
  