# Database Examples

## Run first sqlite example

Create sqlite database:
````````
sqlite3 test.db "create table test (id integer primary key, str text);"
````````

Then build and run:

     
````````
stack build --exec TestSqLite1
````````

## Run using Replit.com, Nix, Cabal

    sqlite3 test.db "create table test (id integer primary key, str text);"
    cabal build
    cabal run