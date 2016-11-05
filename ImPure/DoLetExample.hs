module DoLetExample where
  
example1 = do  -- good style
  putStrLn "Enter an integer number:"
  s <- getLine
  let number = (read s :: Int) + 2
  putStrLn $ "Number plus 2 = " ++ (show number)

example2 = do  -- avoid using "in" inside a do statement
  putStrLn "Enter an integer number:"
  s <- getLine
  let number = (read s :: Int) + 2 in
    putStrLn $ "Number plus 2 = " ++ (show number)

example3 = do  -- avoid using "in" inside a do statement
  putStrLn "Enter an integer number:"
  s <- getLine
  let number = (read s :: Int) + 2 in
    do
      putStrLn "Result is:"
      putStrLn $ "Number plus 2 = " ++ (show number)

main = do
  example1
  example2
  example3