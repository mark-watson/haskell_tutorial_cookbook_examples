module Main where

-- This is the Sprinkler example (with a few changes) from https://github.com/adscib/monad-bayes

import Control.Monad (when)

import Control.Monad.Bayes.Class
import qualified Control.Monad.Bayes.Dist as Dist

enumerate :: Ord a => Dist.Dist Double a -> [(a,Double)]
enumerate = Dist.enumerate

hard :: MonadBayes m => m Bool
hard = do
  rain <- bernoulli 0.3
  sprinkler <- bernoulli $ if rain then 0.1 else 0.4
  wet <- bernoulli $ case (rain,sprinkler) of (True,True) -> 0.98
                                              (True,False) -> 0.8
                                              (False,True) -> 0.9
                                              (False,False) -> 0.0
  condition (wet == False)
  return rain

soft :: MonadBayes m => m Bool
soft = do
  rain <- bernoulli 0.3
  when rain (factor 0.2)
  sprinkler <- bernoulli $ if rain then 0.1 else 0.4
  when sprinkler (factor 0.1)
  return rain
  
main = 
  print $ enumerate hard == enumerate soft
