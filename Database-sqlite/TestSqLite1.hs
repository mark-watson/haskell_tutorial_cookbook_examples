{-# LANGUAGE OverloadedStrings #-} -- allow string literals for multiple string types (e.g., Text)

module Main where
  
import Database.SQLite.Simple -- open, close, query_, execute, Only, fromOnly

{-
   Create sqlite database:
     sqlite3 test.db "create table test (id integer primary key, str text);"

   This example is derived from the example at github.com/nurpax/sqlite-simple
   Program flow: connect → list tables → show schema of 'test' → insert a row → list rows
-}

main :: IO ()
-- program entry point: perform database actions inside IO
main = do
  -- open a connection to the SQLite database file (creates it if missing)
  conn <- open "test.db"
  -- list table names in the database:
  do
    -- `query_` runs a SQL string and returns rows; `Only` wraps a single-column result
    r <- query_ conn "SELECT name FROM sqlite_master WHERE type='table'" :: IO [Only String]
    print "Table names in database test.db:"
    -- `fromOnly` unwraps the single column from `Only`
    mapM_ (print . fromOnly) r
  
  -- get the metadata for table test in test.db:
  do
    -- each row is a single text column containing the table's CREATE statement
    r <- query_ conn "SELECT sql FROM sqlite_master WHERE type='table' and name='test'" :: IO [Only String]
    print "SQL to create table 'test' in database test.db:"
    -- again, use `fromOnly` to unwrap the single column
    mapM_ (print . fromOnly) r
  
  -- add a row to table 'test' and then print out the rows in table 'test':
  do
    -- `execute` runs a parameterized statement; `Only` binds the single placeholder ("?")
    execute conn "INSERT INTO test (str) VALUES (?)"
      (Only ("test string 2" :: String))
    -- query all rows; result type is a tuple matching columns: (id :: Int, str :: String)
    r2 <- query_ conn "SELECT * from test" :: IO [(Int, String)]
    print "number of rows in table 'test':"
    print (length r2)
    print "rows in table 'test':"
    -- `mapM_` applies `print` to each row in the result list
    mapM_ print  r2
    
  -- always close the connection when done
  close conn
  