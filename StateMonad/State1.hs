module Main where

import Control.Monad.State

incrementState :: State Int Int
incrementState = do
  n <- get
  put (n + 1)
  return n

-- same state monad without using a 'do' expression:
incrementState2 :: State Int Int
incrementState2 = get >>= \a ->
                  put (a + 1) >>= \b ->
                  return a

bumpVals (a,b) = (a+1, b+2)

main = do
  print $ runState incrementState 1  -- (1,2) == (return value, final state)
  print $ runState incrementState2 1 -- (1,2) == (return value, final state)
  print $ runState (mapState bumpVals incrementState) 1 -- (2,4)
  print $ evalState incrementState 1  -- 1 == return value
  print $ execState incrementState 1  -- 2 == final state

