module Main where

import CorefWebClient

main :: IO ()
main = do
  putStrLn "Enter text (all on one line)"
  s <- getLine
  response <- corefClient s
  putStr "response from coreference server:\t"
  putStrLn $ show response
  main