module Dice where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
 
uniform_dice = uniformD [1..6]

main = do
  r1 <- uniform_dice
  r2 <- uniform_dice
  print r1
  print r2
      
  