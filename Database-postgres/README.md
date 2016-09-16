# Database Examples

## Test database:

markw=# create database haskell;
CREATE DATABASE
markw=# \c haskell
You are now connected to database "haskell" as user "markw".

create table customers (id int, name text, email text);
CREATE TABLE
haskell=# insert into customers values (1, 'Acme Cement', 'info@acmecement.com');
INSERT 0 1
haskell=# insert into customers values (2, 'Biff Home Sales', 'info@biff.com');
INSERT 0 1
haskell=# insert into customers values (3, 'My Pens', 'info@mypens.com');

markw=# \c haskell
You are now connected to database "haskell" as user "markw".
haskell=# \d
         List of relations
 Schema |   Name    | Type  | Owner 
--------+-----------+-------+-------
 public | customers | table | markw
 public | links     | table | markw
 public | products  | table | markw
(3 rows)

haskell=# select * from customers;
 id |      name       |        email        
----+-----------------+---------------------
  1 | Acme Cement     | info@acmecement.com
  2 | Biff Home Sales | info@biff.com
  3 | My Pens         | info@mypens.com
(3 rows)

haskell=# select * from products;
 id |     name      | cost 
----+---------------+------
  1 | Cement bag    |  2.5
  2 | Cheap Pen     |  1.5
  3 | Expensive Pen | 14.5
(3 rows)

haskell=# select * from links;
 id | customer_id | productid 
----+-------------+-----------
  1 |           1 |         1
  2 |           3 |         2
  3 |           3 |         3
(3 rows)


Then build and run:

     
````````
stack build --exec TestPostgres1
````````

