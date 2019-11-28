module Main where

import System.Random.Shuffle
import Data.List
import Data.Either
import GHC.Generics
import Control.Lens
import Data.Generics.Product.Any
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Read
import Text.Printf
import Data.Maybe

main :: IO ()
main = do
  deck :: [Card] <- shuffleM sunMoonKeyDeck
  game <- initGame
  mainLoop game

mainLoop :: Game -> IO ()
mainLoop game@(Game deck hand tableau) = do
  -- get action from player
  let
    selectActionMenu :: String
    selectActionMenu = printf
      (  "Your hand: %s\n"
      ++ "Select an action:\n"
      ++ "1. Play a Card"
      ++ "2. Discard a Card")
      (intercalate ", " (map show hand))

  actI :: Int <- menuChoice selectActionMenu (1, 2)

  let
    handMenu :: [String]
    handMenu = zipWith
      (\i card -> printf "%s. %s" (show i) (show card))
      [1..]
      hand

    verb :: String
    verb = if actI == 1 then "play" else "discard"

    selectCardMenu :: String
    selectCardMenu = printf
      ("Select a card to %s:\n" ++ intercalate "\n" handMenu)
      verb

  cardI :: Int <- menuChoice selectCardMenu (1, length hand)
  let
    card :: Card
    card = hand !! cardI

    action :: Action
    action = if actI == 1
      then PlaceCard card
      else DiscardCard card

    -- perform action
    -- This really should never fail; it was probably too cautious to
    -- make the takeTurn and other functions Maybe functions
    game' :: Game
    game' = fromJust $ takeTurn action game

    -- draw up to 5
    -- need to draw each card individually and resolve appropriate actions
    -- after each card draw (relevant for drawing Door and Nightmare, and
    -- also relevant for losing the game if the deck is out of cards)

  undefined

menuChoice :: String -> (Int, Int) -> IO Int
menuChoice menu (lo, hi) = do
  putStrLn menu
  choice :: Maybe Int <- readMaybe <$> getLine
  case choice of
    Nothing -> menuChoice menu (lo, hi)
    Just x -> if x >= lo && x <= hi
      then pure x
      else menuChoice menu (lo, hi)

-- Current game state
data Game = Game
  { gameDeck :: [Card]
  , gameHand :: [Card]
  , gameTableau :: [Card]
  } deriving stock (Generic)

data Symbol = Sun | Moon | Key | Door
  deriving (Eq, Show)

data Color = Red | Blue | Green | Brown
  deriving (Eq, Show)

data Card = Card
  { cardColor :: Color
  , cardSymbol :: Symbol
  } deriving (Eq)

instance Show Card where
  show (Card color symbol) = (show color) ++ " " ++ (show symbol)

sameSymbol :: Card -> Card -> Bool
sameSymbol a b = cardSymbol a == cardSymbol b

-- data Player = Player
--   { playerHand :: [Card]
--   , playerTableau :: [Card]
--   } -- deriving stock (Generic)

-- possible actions are placing a card or discarding a card
data Action =
    NoAction
  | PlaceCard Card
  | DiscardCard Card
  | GameLoss
  | DrawDoor Card

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
draw :: Game -> (Action, Game)
draw game@(Game [] _ _) = (GameLoss, game)
draw game@(Game (x:xs) hand tableau) = case cardSymbol x of
  Door -> (DrawDoor x, Game xs hand tableau)
  -- Nightmare -> resolve nightmare
  otherwise -> (NoAction, Game xs (x:hand) tableau)

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
  _ -> \_ -> Nothing
