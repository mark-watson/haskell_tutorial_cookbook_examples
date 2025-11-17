-- Simple web scraper: fetch a page, parse HTML with TagSoup, print headers, text, and links
-- OverloadedStrings lets string literals be `Text` or `ByteString` without explicit packing
{-# LANGUAGE OverloadedStrings #-}

-- HTTP client for making requests
import Network.HTTP.Simple
-- TagSoup: tolerant HTML parser that turns HTML into a list of tags
import Text.HTML.TagSoup
-- Text types and IO helpers
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- Lazy ByteString helpers for response body
import qualified Data.ByteString.Lazy.Char8 as BL8
-- mapMaybe: map and drop Nothings
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
    -- Fetch the HTML content
    response <- httpLBS "https://markwatson.com/"  -- GET request; returns `Response ByteString`
    let body = BL8.unpack $ getResponseBody response  -- convert lazy ByteString to String
        tags = parseTags body                          -- turn HTML into `[Tag String]`

    -- Extract and print headers
    let headers = getResponseHeaders response  -- list of (header-name, value)
    putStrLn "Headers:"
    mapM_ print headers  -- `mapM_` runs `print` over the list in IO

    -- Extract and print all text content
    let texts = extractTexts tags  -- collapse visible text nodes into a single `Text`
    putStrLn "\nText Content:"
    TIO.putStrLn texts  -- use Text IO to print

    -- Extract and print all links
    let links = extractLinks tags  -- grab `href` attributes from <a> tags
    putStrLn "\nLinks:"
    mapM_ TIO.putStrLn links  -- print each link line-by-line

-- Collect visible text from tags and normalize whitespace
extractTexts :: [Tag String] -> Text
extractTexts =
  T.unwords                               -- join words with single spaces
  . map (T.strip . T.pack)                -- trim and convert `String` -> `Text`
  . filter (not . null)                   -- drop empty pieces
  . mapMaybe maybeTagText                 -- keep only text nodes, discard tags

-- Collect `href` values from all <a> tags
extractLinks :: [Tag String] -> [Text]
extractLinks = map (T.pack . fromAttrib "href") . filter isATag
  where
    isATag (TagOpen "a" _) = True   -- match opening <a ...> tag
    isATag _               = False