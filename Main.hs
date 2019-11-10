module Main where

main :: IO ()
main = undefined

data Symbol = Sun | Moon | Key -- | Door

data Color = Red | Blue | Green | Brown

data Card = Card
  { cardColor :: Color
  , cardSymbol :: Symbol
  }

data Player = Player
  { playerHand :: [Card]
  , playerTableau :: [Card]
  }


-- In deck
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

deck :: [Card]
deck = concatMap genCards [Red, Blue, Green, Brown]

redSuns :: [Card]
redSuns = replicate 9 (Card Red Sun)

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
