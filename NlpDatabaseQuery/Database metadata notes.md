# Notes for using database metadata

## Precalculating join data for connecting tables

There are two sources of metadata that I need to use:

- "join" tables lke the table 'links' in the mock data
- foreign keys defined in table schema

The goal is to preprocess this data to create static data to associate data in tables. A motivating example would be the mock test query:

        show all customers with products costing more than $10

select customers.name, products.name from customers join links on links.customer_id = customers.id join products on products.id = links.productId where products.cost > 10.00;

