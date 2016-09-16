module Main where

import Data.Time.Clock.POSIX -- for getPOSIXTime
import System.TimeIt         -- for timeIt
import System.Timeout        -- for timeout

anyCalculationWillDo n =
  take n $ sieve [2..]
            where
              sieve (x:xs) =
                x:sieve [y | y <- xs, rem y x > 0]
                
main = do
  startingTime <- getPOSIXTime
  print startingTime
  print $ last $ take 20000001 [0..]
  endingTime <- getPOSIXTime
  print endingTime
  print (endingTime - startingTime)
  
  timeIt $ print $ last $ anyCalculationWillDo 2000
  let somePrimes = anyCalculationWillDo 3333 in
    timeIt $ print $ last somePrimes
  
  -- 100000 microseconds timeout tests:
  timeout 100000 $ print "simple print statement did not timeout"
  timeout 100000 $ print $ last $ anyCalculationWillDo 4
  timeout 100000 $ print $ last $ anyCalculationWillDo 40
  timeout 100000 $ print $ last $ anyCalculationWillDo 400
  timeout 100000 $ print $ last $ anyCalculationWillDo 4000
  timeout 100000 $ print $ last $ anyCalculationWillDo 40000
  print $ anyCalculationWillDo 5
  
  
