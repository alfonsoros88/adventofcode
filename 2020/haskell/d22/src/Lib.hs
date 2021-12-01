module Lib
  ( Deck
  , emptyDeck
  , parseDecks
  , playGame
  , recursiveGame
  ) where

import Control.Monad.Trans.State.Lazy
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.List (isPrefixOf)
import Data.Maybe
import Text.Read (readMaybe)

newtype Deck =
  Deck [Int]
  deriving (Show, Eq)

instance Hashable Deck where
  hashWithSalt s (Deck xs) = s `hashWithSalt` xs

-- Returns an empty deck
emptyDeck :: Deck
emptyDeck = Deck []

-- Parse a list of strings with numbers into  maybe a list of numbers
-- Example: ["1", "2", "3"] -> Just [1, 2, 3]
parseNumbers :: [String] -> Maybe [Int]
parseNumbers = traverse readMaybe

-- Parses the input into a player deck
-- Example: ["Player 1", "9", "2", "6"] -> Just (Deck [9, 2, 6])
parsePlayerDeck :: [String] -> Maybe Deck
parsePlayerDeck (name:numbers) = Deck <$> parseNumbers numbers

-- Given an input string, splits it at the empty line
-- Example: "Player 1\n9\n\nPlayer 2\n1\n" -> (["Player 1", "9"], ["Player 2", "1"])
splitAtEmptyLine :: String -> ([String], [String])
splitAtEmptyLine input =
  let (before, after) = break (== "") $ lines input
   in (before, dropWhile (== "") after)

-- Given the input string, tries to parse two player decks
-- Example: "Player 1\n9\n\nPlayer 2\n1\n" -> Just (Deck [9], Deck [1])
parseDecks :: String -> Maybe (Deck, Deck)
parseDecks input =
  let (player1, player2) = splitAtEmptyLine input
   in (,) <$> parsePlayerDeck player1 <*> parsePlayerDeck player2

-- Plays one round of the game, where the first card of each player is compared
-- to each other. The winner gets the first card of the other player. The two
-- cards are appended to the back of the winners deck, with the winner card
-- being the first to be appended.
-- Example: (Deck [9, 2, 6], Deck [1, 3, 5]) -> (Deck [2, 6, 9, 1], Deck [3, 5])
-- Example: (Deck [2, 6], Deck [3, 5]) -> (Deck [6], Deck [5, 3, 2])
playRound :: (Deck, Deck) -> (Deck, Deck)
playRound (Deck [], deck) = (Deck [], deck)
playRound (deck, Deck []) = (deck, Deck [])
playRound (Deck (card1:rem1), Deck (card2:rem2)) =
  if card1 > card2
    then (Deck (rem1 ++ [card1, card2]), Deck rem2)
    else (Deck rem1, Deck $ rem2 ++ [card2, card1])

-- Returns True if the deck is empty
isEmpty :: Deck -> Bool
isEmpty (Deck deck) = null deck

-- Calculates the score of a deck. The score is the sum of the value of each
-- card times its rank. The rack of a card is equal to the inverse of its
-- position in the deck. The last card in the deck has the rank 1.
-- Example: Deck [2, 6, 9, 1] -> 45
score :: Deck -> Int
score (Deck deck) =
  let len = length deck
   in sum $ zipWith (*) (reverse [1 .. len]) deck

-- Repeatedly plays a round of the game until one of the decks is empty and
-- returns the score of the winner.
playGame :: (Deck, Deck) -> Int
playGame (Deck deck1, Deck deck2) =
  if isEmpty (Deck deck1)
    then score (Deck deck2)
    else if isEmpty (Deck deck2)
           then score (Deck deck1)
           else playGame (playRound (Deck deck1, Deck deck2))

-- Returns True if the value of the first card of the deck is less or equal to
-- the length of the rest of the deck.
-- Example: Deck [2, 6, 9, 1] -> True
-- Example: Deck [2, 6, 9] -> True
-- Example: Deck [2, 6] -> False
canPlaySubgame :: Deck -> Bool
canPlaySubgame (Deck deck) =
  let len = length deck
   in head deck <= len - 1

-- Returns True of both decks can play a subgame.
-- Example: (Deck [2, 6, 9, 1], Deck [2, 6, 9]) -> True
-- Example: (Deck [2, 6, 9], Deck [2, 6]) -> False
triggerSubgame :: (Deck, Deck) -> Bool
triggerSubgame (d1, d2) = canPlaySubgame d1 && canPlaySubgame d2

alreadyPlayed :: (Deck, Deck) -> State (HashSet.HashSet (Deck, Deck)) Bool
alreadyPlayed conf = do
  played <- get
  put $ HashSet.insert conf played
  return $ HashSet.member conf played

-- Given a deck, returns the corresponding subgame deck from it. The subgame
-- deck is the resulting deck after removing the first card of the deck and
-- taking as many cards from the rest of the deck as the value of the first
-- card.
-- Example: Deck [2, 6, 9, 1] -> Deck [6, 9]
-- Example: Deck [2, 6, 9] -> Deck [6, 9]
subgameDeck :: Deck -> Deck
subgameDeck (Deck deck) = Deck $ take (head deck) (tail deck)

data Winner
  = Player1
  | Player2
  deriving (Show, Eq)

recursiveGame :: (Deck, Deck) -> Int
recursiveGame conf = let (d1, d2) = recursiveGameWithState conf HashSet.empty
  in if isEmpty d1
       then score d2
       else if isEmpty d2
              then score d1
              else error "Game is not over"

recursiveGameWithState ::
     (Deck, Deck) -> HashSet.HashSet (Deck, Deck) -> (Deck, Deck)
recursiveGameWithState conf@(Deck d1, Deck d2) played =
  if HashSet.member conf played
    then (Deck d1, Deck [])
    else let conf'@(d1', d2') =
               if triggerSubgame conf
                 then let (d1', d2') =
                            recursiveGameWithState
                              (subgameDeck (Deck d1), subgameDeck (Deck d2))
                              HashSet.empty
                          c1 = head d1
                          c2 = head d2
                       in if isEmpty d1'
                            then (Deck $ tail d1, Deck $ (tail d2) ++ [c2, c1])
                            else (Deck $ (tail d1) ++ [c1, c2], Deck $ tail d2)
                 else playRound conf
          in if isEmpty d1' || isEmpty d2'
               then conf'
               else recursiveGameWithState conf' (HashSet.insert conf played)
