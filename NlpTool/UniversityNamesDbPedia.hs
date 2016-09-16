-- Copyright 2014 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of the GPL version 3 license.

module UniversityNamesDbPedia (universityMap) where

import qualified Data.Map as M

universityMap = M.fromList [("ISAE formation SUPAERO", "<http://dbpedia.org/resource/%C3%89cole_nationale_sup%C3%A9rieure_de_l'a%C3%A9ronautique_et_de_l'espace>"), ("Telecom Bretagne", "<http://dbpedia.org/resource/%C3%89cole_nationale_sup%C3%A9rieure_des_t%C3%A9l%C3%A9communications_de_Bretagne>"), ("Akita International University", "<http://dbpedia.org/resource/Akita_International_University>"), ("Al-Azhar University", "<http://dbpedia.org/resource/Al-Azhar_University>"), ("American University", "<http://dbpedia.org/resource/American_University>"), ("Babson College", "<http://dbpedia.org/resource/Babson_College>"), ("Berea College", "<http://dbpedia.org/resource/Berea_College>"), ("Bocconi University", "<http://dbpedia.org/resource/Bocconi_University>"), ("Boston College", "<http://dbpedia.org/resource/Boston_College>")]

--  SHORT LIST!! After dev, get old long version from kbnlp.hs project                                                                   

