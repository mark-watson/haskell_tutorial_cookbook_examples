module Main where
  
example3 =  putStrLn "Enter an integer number:" >>  getLine

example4 mv = do
  let number = (read mv :: Int) + 2
  putStrLn $ "Number plus 2 = " ++ (show number)

main = example3 >>= example4
