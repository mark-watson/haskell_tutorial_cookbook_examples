{-# LANGUAGE OverloadedStrings #-}
import Database.SQLite.Simple

{-
   Create sqlite database:
     sqlite3 test.db "create table test (id integer primary key, str text);"

   This example is derived from the example at github.com/nurpax/sqlite-simple
-}

main :: IO ()
main = do
  conn <- open "test.db"
  -- start by getting table names in database:
  do
    r <- query_ conn "SELECT name FROM sqlite_master WHERE type='table'" :: IO [Only String]
    print "Table names in database test.db:"
    mapM_ (print . fromOnly) r
  
  -- get the metadata for table test in test.db:
  do
    r <- query_ conn "SELECT sql FROM sqlite_master WHERE type='table' and name='test'" :: IO [Only String]
    print "SQL to create table 'test' in database test.db:"
    mapM_ (print . fromOnly) r
  
  -- add a row to table 'test' and then print out the rows in table 'test':
  do
    execute conn "INSERT INTO test (str) VALUES (?)"
      (Only ("test string 2" :: String))
    r2 <- query_ conn "SELECT * from test" :: IO [(Int, String)]
    print "number of rows in table 'test':"
    print (length r2)
    print "rows in table 'test':"
    mapM_ print  r2
    
  close conn
  