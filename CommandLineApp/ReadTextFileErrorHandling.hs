module Main where

import System.IO
import Control.Exception

-- catchAny from Michael Snoyman's aticle:
-- (https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions:
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

safeFileReader :: FilePath -> IO String
safeFileReader fPath = do
  entireFileAsString <- catchAny (readFile fPath) $ \error -> do
    putStrLn $ "Error: " ++ show error
    return ""
  return entireFileAsString

main :: IO ()
main = do
  fContents <- safeFileReader "temp.txt"
  print fContents
  print $ words fContents
