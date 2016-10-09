module TestCSV where

import Text.CSV (parseCSVFromFile)
import Data.Either.Unwrap (fromRight)

readCsvFile fname = do
  c <- parseCSVFromFile fname
  return $ fromRight c

main = do
  c <- readCsvFile "test.csv"
  print  c
  print $ map head c
  let header:rows = c
  print header
  print rows

