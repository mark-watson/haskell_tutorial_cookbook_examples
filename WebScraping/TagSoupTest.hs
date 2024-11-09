{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple
import Text.HTML.TagSoup
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    -- Fetch the HTML content
    response <- httpLBS "https://markwatson.com/"
    let body = BL8.unpack $ getResponseBody response
        tags = parseTags body

    -- Extract and print headers
    let headers = getResponseHeaders response
    putStrLn "Headers:"
    mapM_ print headers

    -- Extract and print all text content
    let texts = extractTexts tags
    putStrLn "\nText Content:"
    TIO.putStrLn texts

    -- Extract and print all links
    let links = extractLinks tags
    putStrLn "\nLinks:"
    mapM_ TIO.putStrLn links

-- Function to extract all text content from tags
extractTexts :: [Tag String] -> Text
extractTexts = T.unwords . map (T.strip . T.pack) . filter (not . null) . mapMaybe maybeTagText

-- Function to extract all links from tags
extractLinks :: [Tag String] -> [Text]
extractLinks = map (T.pack . fromAttrib "href") . filter isATag
  where
    isATag (TagOpen "a" _) = True
    isATag _               = False