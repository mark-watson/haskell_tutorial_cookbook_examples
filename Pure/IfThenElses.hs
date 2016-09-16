module Main where

ageToString age =
  if age < 21 then "minor" else "adult"
                                
main = do                                
  print $ ageToString 15
  print $ ageToString 37
  