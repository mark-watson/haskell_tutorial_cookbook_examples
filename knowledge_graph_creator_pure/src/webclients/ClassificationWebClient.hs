{-# LANGUAGE OverloadedStrings #-}

-- reference: http://www.serpentine.com/wreq/tutorial.html
module ClassificationWebClient
  ( classification_client
  ) where

import Control.Lens
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Maybe (fromJust)
import Network.URI.Encode (encode)
import Network.Wreq

base_url = "http://127.0.0.1:8015?text=" -- check - Python code not implemented yet

classification_client :: [Char] -> IO [Char]
classification_client query = do
  let empty = ""
  return empty
