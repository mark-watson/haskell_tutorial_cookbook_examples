{-# LANGUAGE OverloadedStrings #-}

-- example 1 reference: http://www.serpentine.com/wreq/tutorial.html
-- example 2 references: https://github.com/egonSchiele/HandsomeSoup
--                       http://adit.io/posts/2012-04-14-working_with_HTML_in_haskell.html

module Main where

-- for first example:
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens (_String, key)
import Data.Aeson (decode)
import Data.ByteString.Lazy hiding(putStrLn)
import Data.Maybe

-- for second example:
import Text.XML.HXT.Core
import Text.HandsomeSoup

main :: IO ()
main = do
  -- first example:
  r <- get "http://markwatson.com"
  print $ "status code: " ++ (show (r ^. responseStatus . statusCode))
  print $ "url: " ++ (show (r ^? responseBody . key "url"))
  print $ "content type: " ++ (show (r ^? responseHeader "Content-Type"))
  --print $ "body: " ++ (show (r ^? responseBody))
  
  -- second example:
  let doc = fromUrl "http://markwatson.com/"
  links <- runX $ doc >>> css "a" ! "href"
  mapM_ print links
  h2 <- runX $ doc >>> css "h2" ! "href"
  mapM_ print h2
  imageSrc <- runX $ doc >>> css "img" ! "src"
  mapM_ print imageSrc
  allBodyText <- runX $ doc >>> css "body" //> getText
  mapM_ print allBodyText
  pText <- runX $ doc >>> css "p" //> getText -- //> gets al contained text
                                              -- /> gets only directly contained text
  mapM_ print pText
  
  -- third example: JSON from DBPedia
  r <- get "http://dbpedia.org/data/Los_Angeles.json"
  print $ "status code: " ++ (show (r ^. responseStatus . statusCode))
  print $ "url: " ++ (show (r ^? responseBody . key "url"))
  print $ "content type: " ++ (show (r ^? responseHeader "Content-Type"))
  --print $ "json: " ++ (show (decode (fromMaybe r ^? responseBody)))
  