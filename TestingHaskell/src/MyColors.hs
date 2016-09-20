module MyColors where

data MyColors = Orange | Red | Blue | Green | Silver
 deriving (Show, Eq)
          
instance Ord MyColors where
  compare c1 c2 = compare (show c1) (show c2)
