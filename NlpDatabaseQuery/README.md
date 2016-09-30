# Placeholder: this example is not yet implemented

# Database Examples

## Run first sqlite example

Create sqlite database:
````````
sqlite3 test.db "create table test (id integer primary key, str text);"
````````

Then build and run:

     
````````
stack build --exec NlpDatabaseQuery
````````

