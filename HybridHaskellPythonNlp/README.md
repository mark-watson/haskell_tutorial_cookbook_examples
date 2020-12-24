# HybridHaskellPythonNlp

## Start the Python Server

I assume that it is installed (see the book for directions) or see the readme file in the subdirectory python_spacy_nlp_server


## Running the Haskell client

    stack build --fast --exec HybridHaskellPythonNlp-exe

Note: 12/24/2020: to install with json with latest stack resolver, I had to:

set 'allow-newer: true' in ~/.stack/config.yaml