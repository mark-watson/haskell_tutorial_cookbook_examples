module Main where

import NlpWebClient
    
main :: IO ()
main = do
  putStrLn "Enter text (all on one line)"
  s <- getLine
  response <- (nlpClient s) :: IO NlpResponse
  putStr "response from NLP server:\n"
  putStrLn $ show response
  main