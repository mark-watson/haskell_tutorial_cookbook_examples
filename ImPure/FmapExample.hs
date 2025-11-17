module Main where

-- Reads a file and splits on whitespace, returning the list of words.
fileToWords fileName = do
  fileText <- readFile fileName
  return $ words fileText

main = do
  words1 <- fileToWords "text1.txt"
  print $ reverse words1
  words2 <- fmap reverse $ fileToWords "text1.txt"
  print words2

