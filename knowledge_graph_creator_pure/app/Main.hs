-- Entry point: parses command-line args and runs batch processing
-- Usage: run with an input directory and an output file root, e.g.
--   `kgcreator ./test_data out` generates `out.n3` and `out.cypher`
module Main where

import System.Environment (getArgs)
import Apis (processFilesToRdf, processFilesToNeo4j)

main :: IO ()
main
  -- Minimal argument handling: expect 2 args (input dir, output root)
 = do
  args <- getArgs
  case args of
    [] -> error "must supply an input directory containing text and meta files"
    [_] -> error "also specify a root file name for the generated RDF and Cypher files"
    [inputDir, outputFileRoot] -> do
        -- Generate RDF triples (.n3) from input text/meta files
        processFilesToRdf   inputDir $ outputFileRoot ++ ".n3"
        -- Generate Neo4j Cypher (.cypher) from the same input
        processFilesToNeo4j inputDir $ outputFileRoot ++ ".cypher"
    _ -> error "too many arguments"
