# Command Line Utility To Use the Google Gemini APIs

This example is similar to the example in ../webchat but here we build a command line application, not a web application to use the Google Gemini LLM APIs.

Example:

```
$ gemini "what is the square of pi?"
Response:

The square of pi (π) is π multiplied by itself: π².  Since π is approximately 3.14159, π² is approximately 9.8696.
```

The executable file **gemini** is on my path because I copied the executable file to my personal bin directory:

```
$ cabal build
$ find . -name gemini
  ... output not shown
$ cp ./dist-newstyle/build/aarch64-osx/ghc-9.4.8/gemini-0.1.0.0/x/gemini/build/gemini/gemini ~/bin
```

If you don’t want to permanently install this example on your laptop, then just run it with cabal:

```
$ cabal run gemini "what is 11 + 23?"
Response:

11 + 23 = 34
```
