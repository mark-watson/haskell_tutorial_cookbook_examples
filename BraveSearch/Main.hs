{-# LANGUAGE OverloadedStrings #-}

module Main where

import BraveSearch (getSearchSuggestions)
import System.Environment (getEnv)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  -- Get the API key from the environment variable
  apiKey <- getEnv "BRAVE_SEARCH_API_KEY"
  
  -- Prompt the user for a search query
  TIO.putStrLn "Enter a search query:"
  query <- TIO.getLine
  
  -- Call the function to get search suggestions
  result <- getSearchSuggestions apiKey (T.unpack query)
  
  case result of
    Left err -> TIO.putStrLn $ "Error: " <> T.pack err
    Right suggestions -> do
      TIO.putStrLn "Search suggestions:"
      mapM_ (TIO.putStrLn . ("- " <>)) suggestions
