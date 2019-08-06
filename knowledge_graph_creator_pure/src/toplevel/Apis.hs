module Apis
  ( processFilesToRdf
  , processFilesToNeo4j
  ) where

import FileUtils
import GenNeo4jCypher
import GenTriples (textToTriples)

import qualified Database.SQLite.Simple as SQL

import Control.Monad (mapM)
import Data.String.Utils (replace)
import System.Directory (getDirectoryContents)

import Data.Typeable (typeOf)

processFilesToRdf :: FilePath -> FilePath -> IO ()
processFilesToRdf dirPath outputRdfFilePath = do
  files <- getDirectoryContents dirPath :: IO [FilePath]
  let filtered_files = filter isTextFile files
  let full_paths = [dirPath ++ "/" ++ fn | fn <- filtered_files]
  putStrLn "full_paths:"
  print full_paths
  let r =
        [textToTriples fp1 (replace ".txt" ".meta" fp1)
        |
        fp1 <- full_paths] :: [IO [Char]]
  tripleL <-
    mapM (\fp -> textToTriples fp (replace ".txt" ".meta" fp)) full_paths
  let tripleS = concat tripleL
  putStrLn tripleS
  writeFile outputRdfFilePath tripleS

processFilesToNeo4j :: FilePath -> FilePath -> IO ()
processFilesToNeo4j dirPath outputRdfFilePath = do
  files <- getDirectoryContents dirPath :: IO [FilePath]
  let filtered_files = filter isTextFile files
  let full_paths = [dirPath ++ "/" ++ fn | fn <- filtered_files]
  putStrLn "full_paths:"
  print full_paths
  let prelude_node_defs = neo4j_category_node_defs
  putStrLn
    ("+++++  type of prelude_node_defs is: " ++
     (show (typeOf prelude_node_defs)))
  print prelude_node_defs
  cypher_dataL <-
    mapM (\fp -> textToCypher fp (replace ".txt" ".meta" fp)) full_paths
  let cypher_dataS = concat cypher_dataL
  putStrLn cypher_dataS
  writeFile outputRdfFilePath $ prelude_node_defs ++ cypher_dataS
