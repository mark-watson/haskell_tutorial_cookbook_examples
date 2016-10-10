module Main where

import Card   -- pure code
import Table  -- pure code
import RandomizedList  -- impure code

printTable :: Table -> IO ()
printTable aTable =
  putStrLn $ showTable aTable
  
randomDeck =
  randomizedList orderedCardDeck

gameLoop :: Table -> Int -> IO b
gameLoop aTable numberOfPlayers = do
  printTable aTable
  cardDeck <- randomDeck
  if (handOver aTable) then
    do
      putStrLn "\nHand over. State of table at the end of the game:\n"
      printTable aTable
      putStrLn "\nNewly dealt hand:\n"
      gameLoop (initialDeal cardDeck (scoreHands aTable) numberOfPlayers) numberOfPlayers
  else
    do
      putStrLn "Enter command: h)it or set bet to 10, 20, 30; any other key to stay:"
      command <- getLine
      if elem command ["10", "20", "30"] then gameLoop (setPlayerBet (read command) aTable) numberOfPlayers
      else
        if command == "h" then gameLoop (dealCards aTable [0 .. numberOfPlayers]) numberOfPlayers
        else gameLoop (setPlayerPasses (dealCards aTable [1 .. numberOfPlayers])) numberOfPlayers 
             -- player stays (no new cards)
  
main :: IO b
main = do
  print "Start a game of Blackjack. Besides yourself, how many other players do you want at the table?"
  s <- getLine
  let num = (read s :: Int) + 1 -- player indices: 0)user, 1)dealer, and > 1 are the other players
  cardDeck <- randomDeck
  let aTable = initialDeal cardDeck (createNewTable num) num
  gameLoop aTable num
