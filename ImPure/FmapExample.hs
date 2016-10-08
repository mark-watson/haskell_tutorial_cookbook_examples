module FmapExample where

fileToWords fileName = do
  fileText <- readFile fileName
  return $ words fileText
    
main = do
  words1 <- fileToWords "text1.txt"
  print $ reverse words1
  words2 <- fmap reverse $ fileToWords "text1.txt"
  print words2
    
  
