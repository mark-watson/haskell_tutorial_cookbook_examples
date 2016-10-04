module Dice where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler
 
uniform_dice = uniformD [1..6]

roll4 = do
  r1 <- uniform_dice
  r2 <- uniform_dice
  r3 <- uniform_dice
  r4 <- uniform_dice
  return (r1, r2, r3, r4)
                      
main = do
  r1 <- uniform_dice
  r2 <- uniform_dice
  print r1
  print r2
  r4 <- roll4
  print r4
  
      
  