{-# LANGUAGE OverloadedStrings #-}

-- reference: http://www.serpentine.com/wreq/tutorial.html

module HttpClientExample where

-- for first example:
import Network.Wreq
import Control.Lens
--import Data.Aeson.Lens (_String, key)
--import Data.Aeson (decode)
--import Data.ByteString.Lazy hiding(putStrLn)
import Data.Maybe

-- for second example:
import Text.XML.HXT.Core
import Text.HandsomeSoup

-- third example:
import Text.JSON.Generic

main :: IO ()
main = do
  -- JSON from DBPedia
  r <- get "http://dbpedia.org/data/Sedona_Arizona.json"
  putStrLn $ "status code: " ++ (show (r ^. responseStatus . statusCode))
  putStrLn $ "content type: " ++ (show (r ^? responseHeader "Content-Type"))
  putStrLn $ "json: " ++ show (fromJust (r ^? responseBody))
  
  -- note: same data as N3 RDF:  http://dbpedia.org/data/Sedona_Arizona.n3
