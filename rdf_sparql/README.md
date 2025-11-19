# SimpleRDF: In-Memory RDF Store & SPARQL Engine

A lightweight, educational Haskell implementation of an RDF graph store and a basic SPARQL query engine. This project demonstrates how to use the List Monad to perform graph pattern matching and join operations, simulating how SPARQL query execution works.

## Features

- **RDF Data Model**: Supports IRIs, Literals, and Blank Nodes.
- **In-Memory Graph**: Stores triples as a simple list (simulating a database).
- **SPARQL Query Engine**:
  - Supports simple `SELECT` queries.
  - Implements Basic Graph Patterns (BGP).
  - Handles joins automatically via the List Monad.
- **Zero External Heavy Dependencies**: Only uses `base` and `containers`.

## Prerequisites

- [GHC](https://www.haskell.org/ghc/) (The Glasgow Haskell Compiler)
- [Cabal](https://www.haskell.org/cabal/) (Build system)

## How to Run

You can run the project directly using Cabal:

```bash
cabal run
```

Or build and run the executable:

```bash
cabal build
cabal run
```

## Example Usage

The `Main` module comes with a pre-loaded "Social Network" graph and runs three demonstration queries:

1. **Simple Selection**: Find all `?s` that `likes` `?o`.
2. **Filtering**: Find `?who` likes `<Bob>`.
3. **Join Operation**: Find pairs `?a` and `?b` who like each other (reciprocal relationships).

### Sample Output

```text
--- RDF Store Loaded ---
(<Alice>,<likes>,<Bob>)
(<Alice>,<likes>,<Pizza>)
...

--- Query 1: Select ?s ?o where { ?s likes ?o } ---
?s ?o
----------
<Alice> <Bob>
<Alice> <Pizza>
<Bob> <Alice>
...

--- Query 3 (Join): Who likes someone who likes them back? ---
?a ?b
----------
<Alice> <Bob>
<Bob> <Alice>
```

## Code Structure

- **`SimpleRDF.hs`**: The single-file implementation containing:
  - **Data Types**: Definitions for `Node`, `Triple`, and `Graph`.
  - **Query Engine**: `matchTriple` and `evaluatePatterns` logic.
  - **Main**: Example dataset and query runner.

## License

MIT
