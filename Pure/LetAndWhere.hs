module Main where

funnySummation w x y z =
  let bob = w + x
      sally = y + z
  in bob + sally

testLetComprehension =
  [(a,b) | a <- [0..5], let b = 10 * a]

testWhereBlocks a =
  z * q
    where
      z = a + 2
      q = 2

functionWithWhere n  =
  (n + 1) * tenn
  where
    tenn = 10 * n

main = do
  print $ funnySummation 1 2 3 4
  let n = "Rigby"
  print n
  print testLetComprehension
  print $ testWhereBlocks 11
  print $ functionWithWhere 1
  
  
