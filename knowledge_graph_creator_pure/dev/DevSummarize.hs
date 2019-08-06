module Main where

-- this file is just for testing/dev
import Categorize
import FileUtils (filePathToString)
import NlpUtils
import Summarize

import Control.Monad (forM, mapM)
import Data.String.Utils (replace)
import System.Directory (getDirectoryContents)

-- for debug:
import Data.Typeable (typeOf)

testNlp = do
  s1 <- filePathToString "test_data/test1.txt"
  putStrLn "\nSummaries:\n"
  print $ summarize s1
  print $ summarizeS s1
  putStrLn "\nEvidence: categories for this text:"
  let words = splitWords $ cleanText s1
      bestCats = bestCategories words
  print bestCats
  putStrLn "\n"
  s2 <- filePathToString "test_data/test2.txt"
  print $ summarize s2
  print $ summarizeS s2
  putStrLn "\nEvidence: categories for this text:"
  let words = splitWords $ cleanText s2
      bestCats = bestCategories words
  print bestCats
  putStrLn "\n"
  s3 <- filePathToString "test_data/test3.txt"
  print $ summarize s3
  print $ summarizeS s3
  putStrLn "\nEvidence: categories for this text:"
  let words = splitWords $ cleanText s3
      bestCats = bestCategories words
  print bestCats

main :: IO ()
main = do
  putStrLn "DevSummarize:"
  testNlp
