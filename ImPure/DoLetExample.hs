module DoLetExample where
  
example1 = do
  putStrLn "Enter an integer number:"
  s <- getLine
  let number = (read s :: Int) + 2
  putStrLn $ "Number plus 2 = " ++ (show number)

example2 = do
  putStrLn "Enter an integer number:"
  s <- getLine
  let number = (read s :: Int) + 2 in
    putStrLn $ "Number plus 2 = " ++ (show number)

main = do
  example1
  example2