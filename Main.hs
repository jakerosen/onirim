module Main where

import System.Random.Shuffle
import Data.List
import GHC.Generics
import Control.Lens
import Data.Generics.Product.Any

main :: IO ()
main = do
  deck :: [Card] <- shuffleM initDeck
  let
    game :: Game
    game = iterate draw (Game deck [] []) !! 4 -- draw 5 cards
  mainLoop game

mainLoop :: Game -> IO ()
mainLoop game@(Game deck hand tableau) = if gameOver game
  then putStrLn "Game over!"
  else do
    undefined
    -- get action from player
    -- perform action
    -- draw up to 5

-- Current game state
data Game = Game
  { gameDeck :: [Card]
  , gameHand :: [Card]
  , gameTableau :: [Card]
  } deriving stock (Generic)

data Symbol = Sun | Moon | Key -- | Door
  deriving (Eq)

data Color = Red | Blue | Green | Brown
  deriving (Eq)

data Card = Card
  { cardColor :: Color
  , cardSymbol :: Symbol
  } deriving (Eq)

-- data Player = Player
--   { playerHand :: [Card]
--   , playerTableau :: [Card]
--   } -- deriving stock (Generic)

-- possible actions are placing a card or discarding a card
data Action = PlaceCard Card | DiscardCard Card

-- Cards in deck
--
-- Suns
-- 9 Red
-- 8 Blue
-- 7 Green
-- 6 Brown
--
-- Moons
-- 4 Red
-- 4 Blue
-- 4 Green
-- 4 Brown
--
-- Keys
-- 3 Red
-- 3 Blue
-- 3 Green
-- 3 Brown
--
-- Doors
-- 2 Red
-- 2 Blue
-- 2 Green
-- 2 Brown
--
-- 10 Nightmares

-- generate cards of the deck
initDeck :: [Card]
initDeck = concatMap genCards [Red, Blue, Green, Brown]

-- generate cards of this color
genCards :: Color -> [Card]
genCards c =
     replicate n (Card c Sun)
  ++ replicate 4 (Card c Moon)
  ++ replicate 3 (Card c Key)
  -- ++ replicate 2 (Card c Door)
  where
    n :: Int
    n = case c of
      Red -> 9
      Blue -> 8
      Green -> 7
      Brown -> 6

-- checks whether the game is over
-- currently, the game is only over if the deck is empty
gameOver :: Game -> Bool
gameOver (Game deck _ _) = null deck

-- draw a card from the deck, adding it to player hand
draw :: Game -> Game
draw (Game [] _ _) = undefined -- might have to be maybe function
draw (Game (x:xs) hand tableau) = Game xs (x:hand) tableau

-- place card from hand onto tableau or do nothing if the card is not in hand
place :: Card -> Game -> Game
place card game = case find (==card) (game ^. the @"gameHand") of
  -- if card is in game hand
  Nothing -> game -- card wasn't found, do nothing
  Just c -> game -- card found
    & discard c -- discard it from hand
    & the @"gameTableau" %~ (c:) -- add it to tableau
  -- if card `elem` hand
  --   then Game deck (delete card hand) (card:tableau)
  --   else game

-- remove card from hand
discard :: Card -> Game -> Game
discard card = the @"gameHand" %~ delete card

-- take a turn with given action
takeTurn :: Action -> Game -> Game
takeTurn action = case action of
  PlaceCard c -> place c
  DiscardCard c -> discard c
