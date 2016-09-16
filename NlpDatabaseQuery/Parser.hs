module Parser (parse) where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M

import Debug.Trace

data PData = PData { select :: [String],
                     conditional :: [String] } deriving (Show)

stemPlural s =
  if last s == 's' then init s else s
                     
chunkQuery tokens =
  let ind2 = find (\x -> x == "where" || x == "with") tokens in
    case ind2 of
      Just tokenName ->
        let ind = tokenName `elemIndex` tokens
            (x,y) = splitAt (fromJust ind) tokens in
          if length y > 0 then
            PData {select = x, conditional = tail y}
          else PData {select = x, conditional = []}
      Nothing ->  PData {select = tokens, conditional = []}

getTableNames tokens tableL =
  filter
    (\s -> s `elem` tableL || (stemPlural s) `elem` tableL)
    tokens
    
substituteSynonyms tokens synM =
  map
    (\token ->
      case M.lookup token synM of
        Nothing -> token
        Just val -> val)
    tokens

findTableNames tokens tableL =
  filter
    (\token -> token `elem` tokens)
    tableL
     
lookForTableNames :: [String] -> [String] -> [String]
lookForTableNames arguments tableList =
  filter (`elem` tableList) arguments

doShow :: PData->[String]->M.Map String [String]->M.Map String String->[String]
doShow chunks tableList columnMap columnSynonyms =
  let tnames = lookForTableNames (select chunks) tableList
      snames = lookForTableNames (conditional chunks) tableList in
    -- TBD: correlate table names in select and conditional names list !!
    if not (null tnames) then map (\tname -> "select * from '" ++ tname ++ "'") tnames
    else []
  
phelper :: PData -> [String] -> M.Map String [String] -> M.Map String String -> [String]
phelper chunks tableList columnMap columnSynonyms
   | head (select chunks) == "show" = doShow chunks tableList columnMap columnSynonyms
   | otherwise = []
       
parse query tableL columnM synonymsM =
  let tokens = substituteSynonyms (splitOn " " (map toLower query)) synonymsM
      chunks = chunkQuery tokens
      (selTables, condTables) = (findTableNames (select chunks),
                                 findTableNames (conditional chunks))
  in
    phelper chunks tableL columnM synonymsM
