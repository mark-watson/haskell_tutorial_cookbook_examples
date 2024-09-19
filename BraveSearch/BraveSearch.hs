{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BraveSearch
  ( getSearchSuggestions
  ) where

import Network.HTTP.Simple
import Data.Aeson
import qualified Data.Text as T
import Control.Exception (try)
import Network.HTTP.Client (HttpException)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

data SearchResponse = SearchResponse
  { query :: QueryInfo
  , web :: WebResults
  } deriving (Show)

data QueryInfo = QueryInfo
  { original :: T.Text
  } deriving (Show)

data WebResults = WebResults
  { results :: [WebResult]
  } deriving (Show)

data WebResult = WebResult
  { type_ :: T.Text
  , index :: Maybe Int
  , all :: Maybe Bool
  , title :: Maybe T.Text
  , url :: Maybe T.Text
  , description :: Maybe T.Text
  } deriving (Show)

instance FromJSON SearchResponse where
  parseJSON = withObject "SearchResponse" $ \v -> SearchResponse
    <$> v .: "query"
    <*> v .: "web"

instance FromJSON QueryInfo where
  parseJSON = withObject "QueryInfo" $ \v -> QueryInfo
    <$> v .: "original"

instance FromJSON WebResults where
  parseJSON = withObject "WebResults" $ \v -> WebResults
    <$> v .: "results"

instance FromJSON WebResult where
  parseJSON = withObject "WebResult" $ \v -> WebResult
    <$> v .: "type"
    <*> v .:? "index"
    <*> v .:? "all"
    <*> v .:? "title"
    <*> v .:? "url"
    <*> v .:? "description"

getSearchSuggestions :: String -> String -> IO (Either String [T.Text])
getSearchSuggestions apiKey query = do
  let url = "https://api.search.brave.com/res/v1/web/search?q=" ++ query ++ "&country=US&count=5"
  
  request <- parseRequest url
  let requestWithHeaders = setRequestHeader "Accept" ["application/json"]
                         $ setRequestHeader "X-Subscription-Token" [BS.pack apiKey]
                         $ request
  
  result <- try $ httpLBS requestWithHeaders
  
  case result of
    Left e -> return $ Left $ "Network error: " ++ show (e :: HttpException)
    Right response -> do
      let statusCode = getResponseStatusCode response
      if statusCode /= 200
        then return $ Left $ "HTTP error: " ++ show statusCode
        else do
          let body = getResponseBody response
          case eitherDecode body of
            Left err -> return $ Left $ "JSON parsing error: " ++ err
            Right searchResponse@SearchResponse{..} -> do
              let originalQuery = original query
                  webResults = results web
              let suggestions = "Original Query: " <> originalQuery : map formatResult webResults
              return $ Right suggestions

formatResult :: WebResult -> T.Text
formatResult WebResult{..} =
  let titleText = maybe "N/A" ("Title: " <>) title
      urlText = maybe "N/A" ("URL: " <>) url
      descText = maybe "N/A" ("Description: " <>) (fmap (T.take 100) description)
  in T.intercalate " | " [titleText, urlText, descText]
