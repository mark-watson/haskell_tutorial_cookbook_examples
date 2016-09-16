{-# LANGUAGE OverloadedStrings #-}

module Main where
  
import Database.PostgreSQL.Simple

main :: IO ()
main = do
  conn <- connect defaultConnectInfo { connectDatabase = "haskell", connectUser = "markw", connectPassword = "test1"}
  -- start by getting table names in database:
  do
    r <- query_ conn "SELECT name FROM customers" :: IO [(Only String)]
    print "names and emails in table 'customers' in database haskell:"
    mapM_ (print . fromOnly) r
  
  -- add a row to table 'test' and then print out the rows in table 'test':
  do
    let rows :: [(Int, String, String)]
        rows = [(4, "Mary Smith", "marys@acme.com")]
    executeMany conn "INSERT INTO customers (id, name, email) VALUES (?,?,?)" rows
    r2 <- query_ conn "SELECT * from customers" :: IO [(Int, String, String)]
    print "number of rows in table 'customers':"
    print (length r2)
    print "rows in table 'customers':"
    mapM_ print  r2
    
  close conn
  