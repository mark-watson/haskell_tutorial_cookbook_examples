# OpenAI APIs

I am using the library written by Alexander Thiemann at:

    https://github.com/agrafix/openai-hs/tree/main/openai-hs

If I just print:

```
     case result of
       Left failure -> print failure
       Right success -> print $ chrChoices success
```

then the output looks like this:

```
[ChatChoice {chchIndex = 0, chchMessage = ChatMessage {chmContent = Just "Certainly! Here is a simple "Hello, World!" program in Haskell:\n\nhaskell\nmain :: IO ()\nmain = putStrLn \"Hello, World!\"\n\n\nTo run this program, follow these steps:\n\n1. Save the code in a file with a .hs extension, for example, HelloWorld.hs.\n2. Open a terminal and navigate to the directory where you saved the file.\n3. Compile the program using the Glasgow Haskell Compiler (GHC) by running:\n   sh\n   ghc HelloWorld.hs\n   \n4. This will produce an executable file named HelloWorld (or HelloWorld.exe on Windows).\n5. Run the executable by typing:\n   sh\n   ./HelloWorld\n   \n\nYou should see the output:\n\nHello, World!\n", chmRole = "assistant", chmFunctionCall = Nothing, chmName = Nothing}, chchFinishReason = Just "stop"}]
```

If I print:

```
    Right success ->  -- print $ chrChoices success
      case chrChoices success of
        (ChatChoice {chchMessage = ChatMessage {chmContent = content}}:_) ->
          putStrLn $ fromMaybe "No content" $ T.unpack <$> content
        _ -> putStrLn "No choices returned"
```

then the putput looks like this:

```
Certainly! Here's a simple "Hello, World!" program in Haskell:

```haskell
main :: IO ()
main = putStrLn "Hello, World!"
```

To run this program:

1. Save the code in a file, for example, `HelloWorld.hs`.
2. Open a terminal (command prompt).
3. Navigate to the directory where you saved the file.
4. Compile the program using GHC (Glasgow Haskell Compiler):

   ```sh
   ghc --make HelloWorld.hs
   ```

5. Run the compiled program:

   ```sh
   ./HelloWorld
   ```

You should see the output:

```
Hello, World!
```

Alternatively, you can run the Haskell code without compiling by using an interpreter like GHCi (the interactive environment for Haskell):

1. Open a terminal.
2. Start GHCi by typing `ghci`.
3. Load your file with the following command:

   ```sh
   :load HelloWorld.hs
   ```

4. Run the `main` function:

   ```sh
   main
   ```

This should also produce the output "Hello, World!" in the terminal.
```


## Run using Replit.com, Nix, Cabal

    cabal build
    cabal run

Note: I had to manually install:  cabal install cpphs
