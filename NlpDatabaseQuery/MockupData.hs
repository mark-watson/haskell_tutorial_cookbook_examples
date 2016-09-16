module MockupData (synonymsM, tablesL, columnsM) where

import qualified Data.Map as M

synonymsM =
  M.fromList [
    ("info", "information"),
    ("with", "where")
  ]

tablesL = ["customers", "products"]

columnsM =
  M.fromList [("customers", ["id","name", "name"]),
              ("products", ["id", "name", "cost"]),
              ("links", ["id", "customer-ID", "productId"])] -- two types of foreign key naming
{-
Things to work on:
 1. look for combinations of table and column name in queries
		 
	show all customers with products costing more than $10
-}