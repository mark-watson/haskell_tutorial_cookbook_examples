module Card (Card, Rank, Suit, orderedCardDeck, cardValue) where

import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Data.Map (fromList, lookup, keys)

data Card = Card { rank :: Rank
                 , suit :: Suit }
                 deriving (Eq, Show)
                 
data Suit = Hearts | Diamonds | Clubs | Spades
          deriving (Eq, Show, Enum, Ord)

data Rank = Two | Three | Four
          | Five | Six | Seven | Eight
          | Nine | Ten | Jack  | Queen | King | Ace
          deriving (Eq, Show, Enum, Ord)

rankMap = fromList [(Two,2), (Three,3), (Four,4), (Five,5),
                    (Six,6), (Seven,7), (Eight,8), (Nine,9),
                    (Ten,10), (Jack,10), (Queen,10),
                    (King,10), (Ace,11)]

orderedCardDeck :: [Card]
orderedCardDeck = [Card rank suit | rank <- keys rankMap,
                                    suit <- [Hearts .. Clubs]]

cardValue :: Card -> Int
cardValue aCard =
  case (Data.Map.lookup (rank aCard) rankMap) of
    Just n -> n
    Nothing -> 0 -- should never happen
        
