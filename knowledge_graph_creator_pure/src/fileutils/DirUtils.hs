module DirUtils
  ( iterateOverDir
  , iterateOverDirWithFilePattern
  ) where

import Control.Monad (forM, mapM)
import System.Directory (getCurrentDirectory, getDirectoryContents)

iterateOverDir :: FilePath -> ([Char] -> IO b) -> IO ()
iterateOverDir dir_path fn = do
  files <- getDirectoryContents dir_path :: IO [FilePath]
  print files
  let filtered_files = filter (/= "..") $ filter (/= ".") files
  mapM_ fn filtered_files

iterateOverDirWithFilePattern dirPath filterFn fn = do
  files <- getDirectoryContents dirPath :: IO [FilePath]
  print files
  let filtered_files = filter filterFn files
  putStrLn "filtered_files:"
  print filtered_files
  let full_paths = [dirPath ++ "/" ++ fn | fn <- filtered_files]
  putStrLn "full_paths:"
  print full_paths
  mapM_ fn full_paths
