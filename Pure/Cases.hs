module Main where

numberOpinion n = 
  case n of
    0 -> "Too low"
    1 -> "just right"
    _ -> "OK, that is a number"
    
main = do
  print $ numberOpinion 0
  print $ numberOpinion 1
  print $ numberOpinion 2

    