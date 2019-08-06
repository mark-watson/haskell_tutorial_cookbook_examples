# KGCreator

## CorefWebClient Client Example

### Run examples

Main program with fast build option:

~~~~~~~~
stack build --fast --exec KGCreator-exe
stack build --fast --exec "KGCreator-exe test_data outtest"
~~~~~~~~

Main program, recompile everything:

~~~~~~~~
stack build --exec KGCreator-exe
~~~~~~~~

## for Makefile tidy target:

    stack build hindent stylish-haskell

## Problems with Haskero VSCode plugin?

try:

    stack build intero

in this project's directory. This installs intero locally. System side
installation does not seem to work with VSCode and Haskero.

Set key binding to add function type: setup > key bindings, search for haskero add type:

add:

{
    "key": "ctrl+k t",
    "command": "haskero.insertType",
    "when": "editorTextFocus"
}


