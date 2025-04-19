{-# LANGUAGE OverloadedStrings #-}

module Main where

import BraveSearch (getSearchSuggestions)
import qualified Data.ByteString.Char8 as BS
import System.Environment (getEnv)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  -- Get the API key from the environment variable
  -- Read API key from environment and convert to ByteString
  apiKeyRaw <- getEnv "BRAVE_SEARCH_API_KEY"
  let apiKey = BS.pack apiKeyRaw
  
  -- Prompt the user for a search query
  TIO.putStrLn "Enter a search query:"
  query <- TIO.getLine
  
  -- Call the function to get search suggestions
  result <- getSearchSuggestions apiKey query
  
  case result of
    Left err -> TIO.putStrLn $ "Error: " <> err
    Right suggestions -> do
      TIO.putStrLn "Search suggestions:"
      mapM_ (TIO.putStrLn . ("- " <>)) suggestions
