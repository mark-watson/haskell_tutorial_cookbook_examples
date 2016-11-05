module Main where

import qualified Data.Map as M
import Data.Strings (strEndsWith, strStartsWith)
import Data.List (isInfixOf)

import LexiconData

bigram :: [a] -> [[a]]
bigram [] = []
bigram [_] = []
bigram xs = take 2 xs : bigram (tail xs)

containsString word substring = isInfixOf substring word

fixTags twogramList =
  map
  (\[last, current] ->
    -- rule 1: DT, {VBD | VBP} --> DT, NN
    if last !! 1 == "DT" && (current !! 1 == "VBD" || current !! 1 == "VB" || current !! 1 == "VBP")
    then "NN" 
    else
      -- rule 2: convert a noun to a number (CD) if "." appears in the word
      if (current !! 1) !! 0 == 'N' && containsString (current !! 0) "."
      then "CD"
      else
        -- rule 3: convert a noun to a past participle if words.get(i) ends with "ed"
        if (current !! 1) !! 0 == 'N' && strEndsWith (current !! 0) "ed"
        then "VBN"
        else
          -- rule 4: convert any type to adverb if it ends in "ly"
          if strEndsWith (current !! 0) "ly"
          then "RB"
          else
            -- // rule 5: convert a common noun (NN or NNS) to a adjective if it ends with "al"
            if strStartsWith (current !! 1) "NN" && strEndsWith (current !! 1) "al"
            then "JJ"
            else
              -- rule 6: convert a noun to a verb if the preceeding work is "would"
              if strStartsWith (current !! 1) "NN" && (last !! 0) == "would" -- should be case insensitive
              then "VB"
              else
                -- rule 7: if a word has been categorized as a common noun and it ends with "s",
                -- then set its type to plural common noun (NNS)
                if strStartsWith (current !! 1) "NN" && strEndsWith (current !! 0) "s"
                then "NNS"
                else
                  -- rule 8: convert a common noun to a present participle verb (i.e., a gerand)
                  if strStartsWith (current !! 1) "NN" && strEndsWith (current !! 0) "ing"
                  then "VBG"
                  else (current !! 1))
 twogramList
  
substitute tks = bigram $ map tagHelper tks

tagHelper token =
  let tags = M.findWithDefault [] token lexicon in
  if tags == [] then [token, "NN"] else [token, tags]

tag tokens = fixTags $ substitute ([""] ++ tokens)


main = do
  let tokens = ["the", "dog", "ran", "around", "the", "tree", "while",
                "the", "cat", "snaked", "around", "the", "trunk",
                "while", "banking", "to", "the", "left"]
  print $ tag tokens
  print $ zip tokens $ tag tokens

