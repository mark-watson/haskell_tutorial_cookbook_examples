{-# LANGUAGE OverloadedStrings #-}

-- Simple HTTP client to a local NLP service (see wreq tutorial)
module NlpWebClient
  ( nlpClient
  ) where

import Control.Lens
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Maybe (fromJust)
import Network.URI.Encode (encode)
import Network.Wreq

-- Base URL for the NLP server; `query` is URL-encoded and appended
base_url = "http://127.0.0.1:8008?text="

-- Call the NLP server with `query` and return response body as String
nlpClient :: [Char] -> IO [Char]
nlpClient query = do
  putStrLn $ "\n\n***  Processing " ++ query
  r <- get $ base_url ++ (encode query) ++ "&no_detail=1"
  putStrLn $ "status code: " ++ (show (r ^. responseStatus . statusCode))
  putStrLn $ "content type: " ++ (show (r ^? responseHeader "Content-Type"))
  putStrLn $ "response body: " ++ (unpack (fromJust (r ^? responseBody)))
  return $ unpack (fromJust (r ^? responseBody))
