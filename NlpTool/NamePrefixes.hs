-- Copyright 2014 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of the GPL version 3 license.

module NamePrefixes (namePrefixes) where

import qualified Data.Set as S

namePrefixes = S.fromList ["Dr", "Premier", "Major", "King", "General", "Ms", "Gen", "Mrs", "Sen", "Mr", "Doctor", "St", "Prince", "Representative", "Maj", "President", "Congressman", "Vice", "Lt", "Senator"]
