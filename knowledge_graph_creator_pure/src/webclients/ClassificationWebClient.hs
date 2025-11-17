{-# LANGUAGE OverloadedStrings #-}

-- Placeholder HTTP client for a classification service
module ClassificationWebClient
  ( classification_client
  ) where

import Control.Lens
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Maybe (fromJust)
import Network.URI.Encode (encode)
import Network.Wreq

-- Base URL for classification server (implementation TBD)
base_url = "http://127.0.0.1:8015?text=" -- check - Python code not implemented yet

-- Stub: returns empty string until service is implemented
classification_client :: [Char] -> IO [Char]
classification_client query = do
  let empty = ""
  return empty
