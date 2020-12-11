{-# LANGUAGE FlexibleContexts #-}

module Day.Day11 where

import Control.Comonad.Store
import Control.Lens hiding (Empty)
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Debug.Trace
import Linear hiding (trace)
import Safe (headMay)

parseAsciiMap ::
  (Char -> Maybe a) ->
  String ->
  Map (V2 Int) a
parseAsciiMap f = ifoldMapOf (asciiGrid <. folding f) Map.singleton

asciiGrid :: IndexedFold (V2 Int) String Char
asciiGrid = reindexed (uncurry (flip V2)) (lined <.> folded)

data Seat = Empty | Occ | Floor
  deriving (Eq)

instance Show Seat where
  show Empty = "L"
  show Occ = "#"
  show Floor = "."

toSmitte 'L' = Just Empty
toSmitte '#' = Just Occ
toSmitte '.' = Just Floor
toSmitte _ = Nothing

nbs :: V2 Int -> [V2 Int]
nbs v2 = tail $ do
  f <- [id, pred, succ]
  g <- [id, pred, succ]
  pure $ over _x f $ over _y g v2

-- runday :: Map (V2 Int) Seat -> Int
runday grid = if grid == newGrid then grid else runday newGrid
  where
    newGrid = Map.mapWithKey f grid
    f point seat = case seat of
      Floor -> Floor
      Empty -> if occNbs == 0 then Occ else Empty
      Occ -> if occNbs >= 4 then Empty else Occ
      where
        occNbs = length $ filter (== Occ) $ mapMaybe (`Map.lookup` grid) (nbs point)

parse = parseAsciiMap toSmitte

solve1 = Map.size . Map.filter (== Occ) . runday

solve2 = Map.size . Map.filter (== Occ) . runday2

dirs :: V2 Int -> [[V2 Int]]
dirs v2 = do
  dv <- dirVectors
  [tail $ iterate (+ dv) v2]
  where
    dirVectors = tail $ do
      f <- [id, pred, succ]
      g <- [id, pred, succ]
      pure $ over _x f $ over _y g (V2 0 0)

runday2 grid = if grid == newGrid then grid else runday2 newGrid
  where
    newGrid = Map.mapWithKey f grid
    f point seat = case seat of
      Floor -> Floor
      Empty -> if occNbs == 0 then Occ else Empty
      Occ -> if occNbs >= 5 then Empty else Occ
      where
        occNbs = length $ filter (== Occ) $ mapMaybe (join . headMay . dropWhile (== Just Floor) . map (`Map.lookup` grid)) (dirs point)

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print $ solve1 parsed
  print $ solve2 parsed
