{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- reference: http://www.serpentine.com/wreq/tutorial.html
module NlpWebClient
  ( nlpClient, NlpResponse
  ) where

import Control.Lens
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Maybe (fromJust)
import Network.URI.Encode as E -- encode is also in Data.Aeson
import Network.Wreq

import Text.JSON.Generic

data NlpResponse = NlpResponse {entities::[String], tokens::[String]} deriving (Show, Data, Typeable)

base_url = "http://127.0.0.1:8008?text="

nlpClient :: [Char] -> IO NlpResponse
nlpClient query = do
  putStrLn $ "\n\n***  Processing " ++ query
  r <- get $ base_url ++ (E.encode query) ++ "&no_detail=1"
  --putStrLn $ "status code: " ++ (show (r ^. responseStatus . statusCode))
  --putStrLn $ "content type: " ++ (show (r ^? responseHeader "Content-Type"))
  --putStrLn $ "response body: " ++ (unpack (fromJust (r ^? responseBody)))
  let ret = (decodeJSON (unpack (fromJust (r ^? responseBody)))) :: NlpResponse
  return ret
