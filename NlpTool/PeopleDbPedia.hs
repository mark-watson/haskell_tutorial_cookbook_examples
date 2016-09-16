-- Copyright 2014 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of the GPL version 3 license.

module PeopleDbPedia (peopleMap) where

import qualified Data.Map as M

peopleMap = M.fromList [
  ("Aaron Sorkin", "<http://dbpedia.org/resource/Aaron_Sorkin>"),   ("Bill Clinton", "<http://dbpedia.org/resource/Bill_Clinton>"),   ("George W Bush", "<http://dbpedia.org/resource/George_W_Bush>")]

--  SHORT LIST!! After dev, get old long version from kbnlp.hs project

  