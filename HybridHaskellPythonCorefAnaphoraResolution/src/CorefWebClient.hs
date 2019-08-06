{-# LANGUAGE OverloadedStrings #-}

-- reference: http://www.serpentine.com/wreq/tutorial.html
module CorefWebClient
  ( corefClient
  ) where

import Control.Lens
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Maybe (fromJust)
import Network.URI.Encode (encode)
import Network.Wreq

base_url = "http://127.0.0.1:8000?text="

corefClient :: [Char] -> IO [Char]
corefClient query = do
  putStrLn $ "\n\n***  Processing " ++ (encode query)
  r <- get $ base_url ++ (encode query) ++ "&no_detail=1"
  putStrLn $ "status code: " ++ (show (r ^. responseStatus . statusCode))
  putStrLn $ "content type: " ++ (show (r ^? responseHeader "Content-Type"))
  putStrLn $ "response body: " ++ (unpack (fromJust (r ^? responseBody)))
  return $ unpack (fromJust (r ^? responseBody))
