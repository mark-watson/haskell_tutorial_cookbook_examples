# Create database tables for 'haskell':

create table customers (id int primary key, name varchar(30), email varchar(30));
create table products (id int primary key, name varchar(30), cost float);
create table links  (id int primary key, customer_ID int, productId int);

insert into customers values (1, 'Acme Cement', 'info@acmecement.com');
insert into customers values (2, 'Biff Home Sales', 'info@biff.com');
insert into customers values (3, 'My Pens', 'info@mypens.com');

insert into products values (1, 'Cement bag', 2.50);
insert into products values (2, 'Cheap Pen', 1.50);
insert into products values (3, 'Expensive Pen', 14.50);

insert into links values (1, 1, 1);
insert into links values (2, 3, 2);
insert into links values (3, 3, 3);

# Sample NLP queries

## show all products and customers


## show all customers with more than 5 products

I need to associate the table products to customers. The difficult part is identifying link tables that connect tables products and customers.

columnsM =
  M.fromList [("customers", ["id","name", "email"]),
              ("products", ["id", "name", "cost"]),
              ("links", ["id", "customer_ID", "productId"])]
              
select customers.id, customers.name from customers inner join links on links.customer_id = customers.id;

select customers.*, links.*, products.* from customers join links on links.customer_id = customers.id join products on products.id = links.productId;

## show all customers with products costing more than $10

select customers.*, links.*, products.* from customers join links on links.customer_id = customers.id join products on products.id = links.productId where products.cost > 10.00;

select customers.name, products.name from customers join links on links.customer_id = customers.id join products on products.id = links.productId where products.cost > 10.00;

## show all customers with more than 5 products

### Ideas:

- look for tables with just 2 or 3 columns that have column names very similar to other table names

## show products that cost more than $10

select products.name from products where products.cost > 10.00;

