{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Text.JSON.Generic
          
data Person = Person {name::String, email::String } deriving (Show, Data, Typeable)

main = do
  let a = encodeJSON $ Person "Sam" "sam@a.com"
  print a
  --let d = (decodeJSON a :: Person)
  let d = (decodeJSON a)
  print d
  print $ name d
  print $ email d

  