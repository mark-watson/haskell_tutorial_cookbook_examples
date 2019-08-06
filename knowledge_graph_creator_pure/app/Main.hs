module Main where

import System.Environment (getArgs)
import Apis (processFilesToRdf, processFilesToNeo4j)

main :: IO ()
main
  --  TBD: add command line argument processing
 = do
  args <- getArgs
  case args of
    [] -> error "must supply an input directory containing text and meta files"
    [_] -> error "in addition to an input directory, also specify a root file name for the generated RDF and Cypher files"
    [inputDir, outputFileRoot] -> do
        processFilesToRdf   inputDir $ outputFileRoot ++ ".n3"
        processFilesToNeo4j inputDir $ outputFileRoot ++ ".cypher"
    _ -> error "too many arguments"
