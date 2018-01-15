module Main where

import Debug.Trace  (trace, traceShow) -- for debugging only!

anyCalculationWillDo n =
  trace
      ("+++ anyCalculationWillDo: " ++ show n) $
      anyCalculationWillDo' n

anyCalculationWillDo' n =
  take n $ trace ("   -- sieve n:" ++ (show n)) $ sieve [2..]
            where
              sieve (x:xs) =
                  traceShow ("     -- inside sieve recursion") $
                            x:sieve [y | y <- xs, rem y x > 0]
                
main = do
  print $ anyCalculationWillDo 5
  
  
