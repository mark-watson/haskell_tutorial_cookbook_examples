{-# LANGUAGE TemplateHaskell #-}  -- for makeLens

module Table (Table (..), createNewTable, setPlayerBet, showTable, initialDeal,
              changeChipStack, setCardDeck, dealCards, resetTable, scoreHands,
              dealCardToUser, handOver, setPlayerPasses) where  -- note: export dealCardToUser only for ghci development

import Control.Lens

import Card
import Data.Bool
import Data.Maybe (fromMaybe)

data Table = Table { _numPlayers        :: Int
                   , _chipStacks       :: [Int] -- number of chips, indexed by player index
                   , _dealtCards       :: [[Card]] -- dealt cards for user, dealer, and other players
                   , _currentPlayerBet :: Int
                   , _userPasses       :: Bool
                   , _cardDeck         :: [Card]
                   }
           deriving (Show)
           
type Players = Int
             
createNewTable :: Players -> Table
createNewTable n =
  Table n
        [500 | _ <- [1 .. n]] -- give each player (incuding dealer) 10 chips
        [[] | _ <- [0..n]] -- dealt cards for user and other players (we don't track dealer's chips)
        20 -- currentPlayerBet
        False
        [] -- placeholder for random shuffled card deck
 
resetTable :: [Card] -> Table -> Int -> Table
resetTable cardDeck aTable numberOfPlayers =
  Table numberOfPlayers
        (_chipStacks aTable)
        [[] | _ <- [0..numberOfPlayers]]
        (_currentPlayerBet aTable)
        False
        cardDeck
     
     -- Use lens extensions:
            
makeLenses ''Table
 
showDealtCards :: [[Card]] -> String
showDealtCards dc =
  (show [map cardValue hand | hand <- dc])

setCardDeck :: [Card] -> Table -> Table
setCardDeck newDeck =
  over cardDeck (\_ -> newDeck)  

dealCards :: Table -> [Int] -> Table
dealCards aTable playerIndices =
  last $ scanl dealCardToUser aTable playerIndices
 
initialDeal cardDeck aTable numberOfPlayers =
  dealCards
    (dealCards (resetTable cardDeck aTable numberOfPlayers) [0 .. numberOfPlayers])
    [0 .. numberOfPlayers]
    
showTable :: Table -> [Char]
showTable aTable =
  "\nCurrent table data:\n" ++
  "  Chipstacks: " ++
  "\n    Player: " ++ (show (head (_chipStacks aTable))) ++
  "\n    Other players: " ++ (show (tail (_chipStacks aTable))) ++
  "\n  User cards: " ++ (show (head (_dealtCards aTable))) ++
  "\n  Dealer cards: " ++ (show ((_dealtCards aTable) !! 1)) ++
  "\n  Other player's cards: " ++ (show (tail (tail(_dealtCards aTable)))) ++
  -- "\n  Dealt cards: " ++ (show (_dealtCards aTable)) ++
  "\n  Dealt card values: " ++ (showDealtCards (_dealtCards aTable)) ++
  "\n  Current player bet: " ++
  (show (_currentPlayerBet aTable)) ++
  "\n  Player pass: " ++
  (show (_userPasses aTable)) ++ "\n"
  
clipScore aTable playerIndex =
  let s = score aTable playerIndex in
    if s < 22 then s else 0
      
scoreHands aTable =
  let chipStacks2 = _chipStacks aTable
      playerScore = clipScore aTable 0
      dealerScore = clipScore aTable 1
      otherScores = map (clipScore aTable) [2..]
      newPlayerChipStack = if playerScore > dealerScore then
                             (head chipStacks2) + (_currentPlayerBet aTable)
                           else
                             if playerScore < dealerScore then
                                (head chipStacks2) - (_currentPlayerBet aTable)
                             else (head chipStacks2)
      newOtherChipsStacks =
        map (\(x,y) -> if x > dealerScore then
                         y + 20
                       else
                         if x < dealerScore then
                           y - 20
                         else y) 
            (zip otherScores (tail chipStacks2))
      newChipStacks  = newPlayerChipStack:newOtherChipsStacks
  in
    over chipStacks (\_ -> newChipStacks) aTable
     
setPlayerBet :: Int -> Table -> Table
setPlayerBet newBet =
  over currentPlayerBet (\_ -> newBet)  

setPlayerPasses :: Table -> Table
setPlayerPasses aTable =
  let numPlayers = _numPlayers aTable
      playerIndices = [1..numPlayers]
      t1 = over userPasses (\_ -> True) aTable
      t2 = dealCards t1 playerIndices
      t3 = dealCards t2 playerIndices
      t4 = dealCards t3 playerIndices
  in
    t4
    
    
changeChipStack :: Int -> Int -> Table -> Table
changeChipStack playerIndex newValue =
  over chipStacks (\a -> a & element playerIndex .~ newValue)

scoreOLD aTable playerIndex =
  let scores = map cardValue ((_dealtCards aTable) !! playerIndex)
      totalScore = sum scores in
    if totalScore < 22 then totalScore else 0

score aTable playerIndex =
  let scores = map cardValue ((_dealtCards aTable) !! playerIndex)
      totalScore = sum scores in
    totalScore
  
dealCardToUser' :: Table -> Int -> Table
dealCardToUser' aTable playerIndex =
  let nextCard = head $ _cardDeck aTable
      playerCards = nextCard : ((_dealtCards aTable) !! playerIndex)
      newTable = over cardDeck (\cd -> tail cd) aTable in
    over dealtCards (\a -> a & element playerIndex .~ playerCards) newTable

dealCardToUser :: Table -> Int -> Table
dealCardToUser aTable playerIndex
  | playerIndex == 0  = dealCardToUser' aTable playerIndex -- user
  | otherwise         = if (score aTable playerIndex) < 17 then
                             dealCardToUser' aTable playerIndex
                        else aTable
  
handOver :: Table -> Bool
handOver aTable =
  _userPasses aTable

