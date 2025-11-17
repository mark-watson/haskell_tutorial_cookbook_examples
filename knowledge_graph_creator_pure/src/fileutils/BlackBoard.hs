{-# LANGUAGE OverloadedStrings #-}

-- Minimal SQLite-backed key store used during processing/debugging
module BlackBoard
  ( blackboard_init
  , blackboard_write
  , blackboard_check_key
  ) where

import Data.ByteString.Lazy.Char8 ()
import Data.Monoid ((<>), mappend, mempty)
import Database.SQLite.Simple

import qualified Database.SQLite.Simple as SQL

-- for debug:
import Data.Typeable (typeOf)

-- Create (or reset) the `blackboard` table in `temp.db`
blackboard_init = do
  conn <- SQL.open "temp.db"
  putStrLn "Creating table blackboard"
  execute_ conn "drop table if exists blackboard;"
  execute_ conn "create table blackboard (key text);"
  close conn

-- Insert a single key into the blackboard
blackboard_write key = do
  conn <- SQL.open "temp.db"
  execute conn "INSERT INTO blackboard (key) VALUES (?)" (Only (key :: String))
  close conn

-- Check if a key exists in the blackboard
blackboard_check_key key = do
  conn <- SQL.open "temp.db"
  r <-
    query conn "SELECT key FROM blackboard WHERE key = ?" (Only (key :: String)) :: IO [Only String]
  close conn
  return ((length r) > 0)
