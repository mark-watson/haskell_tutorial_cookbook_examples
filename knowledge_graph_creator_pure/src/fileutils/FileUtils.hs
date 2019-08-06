{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module FileUtils
  ( filePathToWordTokens
  , filePathToString
  , readMetaFile
  , uri
  , MyMeta
  , isTextFile
  , isMetaFile
  ) where

import Control.Monad
import Data.List
import System.IO
import Data.Char (toLower)

import Database.SQLite.Simple

import Text.JSON.Generic

import NlpUtils (splitWordsKeepCase)

filePathToWordTokens :: FilePath -> IO [String]
filePathToWordTokens file_path = do
  handle <- openFile file_path ReadMode
  contents <- hGetContents handle
  let some_words = splitWordsKeepCase contents
  return some_words

filePathToString file_path = do
  handle <- openFile file_path ReadMode
  contents <- hGetContents handle
  return contents

data MyMeta =
  MyMeta
    { uri :: String,
      similar_docs :: [String]
    }
  deriving (Show, Data, Typeable)

readMetaFile :: [Char] -> IO MyMeta
readMetaFile file_path = do
  putStr $ concat ["++ readMetaFile ", show file_path, "\n"]
  handle <- openFile file_path ReadMode
  s <- (hGetContents handle)
  let meta = (decodeJSON s :: MyMeta)
  return meta
  -- unused for now, but may be used later:

findSubstring pat str = findIndex (isPrefixOf pat) (tails str)

isTextFile file_name = (findSubstring ".txt" file_name) /= Nothing

isMetaFile file_name = (findSubstring ".meta" file_name) /= Nothing

-- utilities from vmchale's hgis project:

stripFileExtension = reverse . drop 1 . dropWhile (/= '.') . reverse

getFileExtension = fmap toLower . reverse . takeWhile (/= '.') . reverse
