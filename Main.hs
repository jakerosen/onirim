module Main where

import System.Random.Shuffle
import Data.List
import Data.Either
import GHC.Generics
import Control.Lens
import Data.Generics.Product.Any

main :: IO ()
main = do
  deck :: [Card] <- shuffleM sunMoonKeyDeck
  game <- initGame
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

data Symbol = Sun | Moon | Key | Door
  deriving (Eq)

data Color = Red | Blue | Green | Brown
  deriving (Eq)

data Card = Card
  { cardColor :: Color
  , cardSymbol :: Symbol
  } deriving (Eq)

sameSymbol :: Card -> Card -> Bool
sameSymbol a b = cardSymbol a == cardSymbol b

-- data Fail = Fail

-- data Player = Player
--   { playerHand :: [Card]
--   , playerTableau :: [Card]
--   } -- deriving stock (Generic)

-- possible actions are placing a card or discarding a card
data Action = PlaceCard Card | DiscardCard Card | GameLoss

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
sunMoonKeyDeck :: [Card]
sunMoonKeyDeck = concatMap genCards [Red, Blue, Green, Brown]

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

addNMDoors :: [Card] -> [Card]
addNMDoors deck =
  (replicate 2 (flip Card Door) <*> [Red, Blue, Green, Brown])
  -- ++ nightmares
  ++ deck

initGame :: IO Game
initGame = do
  deck :: [Card] <- shuffleM sunMoonKeyDeck
  let
    hand :: [Card]
    hand = take 5 deck
    deck' = deck -- temp; only for non-Door version
  -- deck' [Card] <- shuffleM (addNMDoors deck)
  pure (Game deck' hand [])

-- checks whether the game is over
-- currently, the game is only over if the deck is empty
gameOver :: Game -> Bool
gameOver (Game deck _ _) = null deck

-- draw a card from the deck, adding it to player hand
draw :: Game -> Either Action Game
draw (Game [] _ _) = Left GameLoss
draw (Game (x:xs) hand tableau) = Right $ Game xs (x:hand) tableau

-- place card from hand onto tableau or do nothing if the card is not in hand
place :: Card -> Game -> Maybe Game
place card game = do
  card' <- find (==card) (gameHand game)
  -- symbol of card cannot match symbol of Tableau head
  let
    lastCard :: Card
    lastCard = (head (gameTableau game))
  if sameSymbol card' lastCard
    then Nothing -- if same symbol as last card, don't play it
    else Just $ game -- if different symbol, play it
      & the @"gameHand" %~ delete card' -- discard it from hand
      & the @"gameTableau" %~ (card':) -- add it to tableau

-- remove card from hand
discard :: Card -> Game -> Maybe Game
discard card game = do
  card' <- find (==card) (gameHand game)
  Just (game & the @"gameHand" %~ delete card')

-- take a turn with given action
takeTurn :: Action -> Game -> Maybe Game
takeTurn action = case action of
  PlaceCard c -> place c
  DiscardCard c -> discard c
  _ -> undefined
