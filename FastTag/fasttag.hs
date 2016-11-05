import qualified Data.Map as M

import LexiconData

-- test mini-lexicon:

--lexicon = M.fromList[("axis",["NN"]), ("According", ["VBG","NNP"]),
-- ("the", ["DT", "VBD"]), ("dog", ["NN", "VBP"]), ("ran", ["VBD"]),
-- ("around",["IN","RB","RP"]), ("tree",["NN"])]

bigram :: [a] -> [[a]]
bigram [] = []
bigram [_] = []
bigram xs = take 2 xs : bigram (tail xs)

endsWith suffix2 s = last s == suffix2 !! 1 && (last $ init s) == suffix2 !! 0

startsWith word substring = (take 2 word) == substring

containsString word substring = False

fixTagsSIMPLE twogramList =
  map
  (\[last, current] -> current !! 1)
  twogramList

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
        if (current !! 1) !! 0 == 'N' && endsWith "ed" (current !! 0)
        then "VBN"
        else
          -- rule 4: convert any type to adverb if it ends in "ly"
          if endsWith "ly" (current !! 0)
          then "RB"
          else
            -- // rule 5: convert a common noun (NN or NNS) to a adjective if it ends with "al"
            if startsWith (current !! 1) "NN" && endsWith (current !! 1) "al"
            then "JJ"
            else
              -- rule 6: convert a noun to a verb if the preceeding work is "would"
              if startsWith (current !! 1) "NN" && (last !! 0) == "would" -- should be case insensitive
              then "VB"
              else
                -- rule 7: if a word has been categorized as a common noun and it ends with "s",
                -- then set its type to plural common noun (NNS)
                if startsWith (current !! 1) "NN" && endsWith (current !! 0) "s"
                then "NNS"
                else
                  -- rule 8: convert a common noun to a present participle verb (i.e., a gerand)
                  if startsWith (current !! 1) "NN" && endsWith (current !! 0) "ing"
                  then "VBG"
                  else (current !! 1))
 twogramList
  
substitute tks = bigram $ map tagHelper tks

tagHelper token =
  let tags = M.findWithDefault [] token lexicon in
  if tags == [] then [token, "NN"] else [token, tags]

tag tokens = fixTags $ substitute ([""] ++ tokens)


main = do
  print $ M.findWithDefault []  "axis" lexicon
  print $ M.findWithDefault []  "axis213" lexicon
  print $ tagHelper "dog"
  print $ tagHelper "ran"
  let tokens = ["the", "dog", "ran", "around", "the", "tree"]
  print $ fixTags $ substitute tokens
  print $ tag tokens
  print $ fixTags [[["",""], ["the","DT"]],[["the","DT"],["dog","NN"]],[["dog","NN"],["ran","VBD"]],[["ran","VBD"],["around","IN"]],[["around","IN"],["the","DT"]],[["the","DT"],["tree","NN"]]]

