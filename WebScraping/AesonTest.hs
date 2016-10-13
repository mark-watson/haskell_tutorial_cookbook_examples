{-# LANGUAGE OverloadedStrings #-}

-- reference: http://www.serpentine.com/wreq/tutorial.html

module AesonTest where

-- for first example:
import Network.Wreq -- for 'get'
import Control.Lens
import Data.Aeson.Lens (_String, key)
import Data.Aeson (decode)
import Data.ByteString.Lazy hiding(putStrLn)
import Data.Maybe

main :: IO ()
main = do
  r <- get "http://markwatson.com"
  putStrLn $ "status code: " ++ (show (r ^. responseStatus . statusCode))
  putStrLn $ "content type: " ++ (show (fromJust (r ^? responseHeader "Content-Type")))
  putStrLn $ "body: " ++ (show (fromJust (r ^? responseBody)))
  
