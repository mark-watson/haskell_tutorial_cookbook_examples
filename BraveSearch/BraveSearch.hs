{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BraveSearch
  ( getSearchSuggestions
  ) where

import Network.HTTP.Simple
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson
import qualified Data.Text as T
import Control.Exception (try)
import Network.HTTP.Client (HttpException)
import qualified Data.ByteString.Char8 as BS
-- removed unused import Data.ByteString.Lazy.Char8

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

-- | Perform a Brave Search with the given API key (as raw bytes) and text query.
getSearchSuggestions :: BS.ByteString -> T.Text -> IO (Either T.Text [T.Text])
getSearchSuggestions apiKey query = do
  -- Build base request
  let baseUrl = "https://api.search.brave.com/res/v1/web/search"
  request0 <- parseRequest baseUrl
  -- Add query parameters (URL-encoded) and headers
  let request1 = setRequestQueryString
                   [ ("q", Just $ encodeUtf8 query)
                   , ("country", Just "US")
                   , ("count", Just "5")
                   ]
                   request0
      request  = setRequestHeader "Accept" ["application/json"]
               $ setRequestHeader "X-Subscription-Token" [apiKey]
               $ request1

  result <- try $ httpLBS request

  case result of
    Left e -> return . Left $ T.pack $ "Network error: " ++ show (e :: HttpException)
    Right response ->
      let status = getResponseStatusCode response
      in if status /= 200
           then return . Left $ T.pack $ "HTTP error: " ++ show status
           else case eitherDecode (getResponseBody response) of
                  Left err -> return . Left $ T.pack $ "JSON parsing error: " ++ err
                  Right SearchResponse{..} ->
                    let originalQuery = original query
                        webResults    = results web
                        suggestions   = ("Original Query: " <> originalQuery)
                                      : map formatResult webResults
                    in return $ Right suggestions

formatResult :: WebResult -> T.Text
formatResult WebResult{..} =
  let titleText = maybe "N/A" ("Title: " <>) title
      urlText = maybe "N/A" ("URL: " <>) url
      descText = maybe "N/A" ("Description: " <>) (fmap (T.take 100) description)
  in T.intercalate " | " [titleText, urlText, descText]
