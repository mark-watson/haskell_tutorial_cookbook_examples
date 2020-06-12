# Examples for "Haskell Tutorial and Cookbook, Second Edition" by Mark Watson

![Haskell Logo](haskell.svg)

This repo contains the examples for my Haskell book:

[Haskell Tutorial and Cookbook](https://leanpub.com/haskell-cookbook)

Updated 6/12/2020 for stack resolver: lts-16.0

Updated 8/6/2019 for the second edition of my book

## Book Table of Contents (This will change as the book is written)

````````
Contents
Cover Material, Copyright, and License
Preface
 A Request from the Author
 Structure of the Book
 Code Examples
 Functional Programming Requires a Different Mind Set
 Why Haskell?
 
Section 1 - Tutorial

Tutorial on Pure Haskell Programming
  Interactive GHCi Shell
  Using Parenthesis or the Special $ Character and Operator Precedence  
  Lazy Evaluation
  Understanding List Comprehensions 
  Haskell Rules for Indenting Code
  Understanding let and where 
  Conditional Statements and Anonymous Functions 
  Maps
  Sets
  More on Functions
  Comments on Dealing With Immutable Data and How to Structure Programs
  Error Handling
  Testing Haskell Code
 
Tutorial on Impure Haskell Programming
  Hello World Simple Main Program 
  Console IO
  FileIO
  Error Handling in Impure Code
  Network IO
  A More Detailed Look at Monads
  List Comprehensions Using the do Notation
  Using Mutable Maps
  Mutable State Using the State Monad
  Database Access for Sqlite
  Database Access for Postgres
  Dealing With Time
  Writing CommandLine Applications in Haskell

Section 2 - Cookbook

  Text Processing
  CSV Spreadsheet Files
  JSON Data
  Cleaning Natural Languge Text
  Natural Language Processing Tools
  Resolve Entities in Text to DBPedia URIs
  Bag of Words Classification Model
  Text Summarization
  Web Scraping
  Haskell Libraries
  Linked Data and the Semantic Web
  Play a simple version of the Blackjack card game

Section 3 - More applications

  Knowledge Graph Creator (convert plain text to Neo4J and RDF data)
  Hybrid Haskell and Python NLP example using SpaCy NLP library
  Hybrid Haskell and Python Anaphora Resolution (coreference detection) example

Appendix A - Haskell Tools Setup

  Stack
  Emacs Setup
  Do you want more of an IDE-like Development Environment?
  hlint
````````

## Tool Installation

See Appendix A in my book, or simply install **stack** and **hlint**.

## Alternative license for the Knowledge Graph Creator example

All examples in this book are licensed using the Apache 2 licence except for the Knowledge Graph Creator example which is released under the AGPL version 3 license. If you need to use the KGCreator code in a commercial application I offer an alternative license for $50 (see [my open source web page](https://markwatson.com/opensource/)).
