module Main where

head' (x:_) = x
tail' (_:xs) = xs

doubleList [] = []
doubleList (x:xs) = (* 2) x : doubleList xs

bumpList n [] = []
bumpList n (x:xs) = n * x : bumpList n xs

map' f [] = []
map' f (x:xs) = (f x) : map' f xs

main = do
  print $ head' ["bird","dog","cat"]
  print $ tail' [0,1,2,3,4,5]
  print $ doubleList [0..5]
  print $ bumpList 3 [0..5]
  print $ map' (* 7) [0..5]
  print $ map' (+ 1.1) [0..5]
  print $ map' (\x -> (x + 1) * 2) [0..5]

  
  