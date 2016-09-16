-- Copyright 2014 by Mark Watson. All rights reserved. The software and data in this project can be used under the terms of the GPL version 3 license.

{-# LANGUAGE ScopedTypeVariables,OverloadedStrings #-}

-- this is a throw away file. I was just learning how to combine results from Open Calais and a sample SPARQL client.

module Combo where

import Sparql2 (doit)
import OpenCalais (calaisResults)

main = do
  c <- calaisResults "Berlin Germany visited by George W. Bush to see IBM plant. Bush met with President Clinton. Bush said “felt it important to crank up”"
  s <- doit
  print c
  print $ c !! 0
  print $ c !! 1
  print s
  print $ s !! 0
  print $ s !! 1

  
