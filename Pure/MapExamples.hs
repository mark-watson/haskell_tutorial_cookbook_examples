module MapExamples where

import qualified Data.Map as M -- from library containers


aTestMap = M.fromList [("height", 120), ("weight", 15)]

getNumericValue key aMap =
  case M.lookup key aMap of
    Nothing -> -1
    Just value -> value

main = do
  print $ getNumericValue "height" aTestMap
  print $ getNumericValue "age" aTestMap

