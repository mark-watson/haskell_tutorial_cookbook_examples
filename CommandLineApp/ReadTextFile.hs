module Main where
  
import System.IO
import Control.Monad

main = do
  entireFileAsString <- readFile "temp.txt"
  print entireFileAsString
  let allWords = words entireFileAsString
  print allWords