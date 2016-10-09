{-# LANGUAGE DeriveGeneric #-}

module TestJSON where

import Data.Aeson
import GHC.Generics
import Data.Maybe

data Person = Person {name::String, email::String } deriving (Show, Generic)

-- nice trick from School Of Haskell tutorial on Aeson:
instance FromJSON Person  -- DeriveGeneric language setting allows
instance ToJSON Person    -- automatic generation of instance of
                          -- types deriving Generic.

main = do
  let a = encode $ Person "Sam" "sam@a.com"
  print a
  let (Just d) = (decode a :: Maybe Person)
  print d
  print $ name d
  print $ email d

