# Simple implementation of Blackjack game

The are a few limitations in this implementation:

- Aces always count as 11 points (instead of 11 or 1)
- The command line user interface does not hide "down cards" beecause it is intended to show internal state of the game


## Running game

stack build --exec Blackjack

Notes:

- Start by entering the number of players (besides the game user) at the table (a good value is 1)
- In the main game loop, you can enter one of:
--  10, 20, 30: change the user's bet by typing a number
--  h: hit (user gets dealt a card) and the dealer and other player hit if they have < 17
--  blank line: user passes and other players and dealer keep hitting until that have > 16 or they bust (have > 21)

After a user pass, just start a new game by hitting 'h', etc.

## Maintain state of game without using a State Monad

Solution, I used my code for the "Game Loop" pattern from my book [Haskell Tutorial and Cookbook](https://leanpub.com/haskell-cookbook).

### Player Chips

Starting value of 10, can be changed during the game.

## Data Transformations

I maintain a read-only value for a Table value. Many functions take a Table and return a modified Table.


## Interactive development

  -- After 'stack ghci' the following is useful during development:
  
--  let cardDeck = [Card {rank = Eight, suit = Clubs},Card {rank = Three, suit = Hearts},Card {rank = Ace, suit = Hearts},Card {rank = Six, suit = Hearts},Card {rank = Ace, suit = Clubs},Card {rank = Jack, suit = Diamonds},Card {rank = Nine, suit = Clubs},Card {rank = Two, suit = Clubs},Card {rank = Ten, suit = Hearts},Card {rank = Jack, suit = Clubs},Card {rank = Five, suit = Clubs},Card {rank = Four, suit = Diamonds},Card {rank = Queen, suit = Clubs},Card {rank = King, suit = Diamonds},Card {rank = Ace, suit = Diamonds},Card {rank = Nine, suit = Diamonds},Card {rank = Eight, suit = Hearts},Card {rank = Ten, suit = Diamonds},Card {rank = King, suit = Hearts},Card {rank = Queen, suit = Diamonds},Card {rank = Four, suit = Hearts},Card {rank = Seven, suit = Hearts},Card {rank = King, suit = Clubs},Card {rank = Ten, suit = Clubs},Card {rank = Jack, suit = Hearts},Card {rank = Three, suit = Clubs},Card {rank = Two, suit = Hearts},Card {rank = Seven, suit = Diamonds},Card {rank = Nine, suit = Hearts},Card {rank = Eight, suit = Diamonds},Card {rank = Three, suit = Diamonds},Card {rank = Four, suit = Clubs},Card {rank = Queen, suit = Hearts},Card {rank = Seven, suit = Clubs},Card {rank = Two, suit = Diamonds},Card {rank = Six, suit = Clubs},Card {rank = Five, suit = Diamonds},Card {rank = Six, suit = Diamonds},Card {rank = Five, suit = Hearts}]
--  let tt = setCardDeck cardDeck (createNewTable 2)
--  let tt2 = dealCardToUser tt 1
--  setPlayerBet 99 tt
--  changeChipStack 2 88 tt
--  _chipStacks tt
