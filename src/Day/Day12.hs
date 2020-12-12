module Day.Day12 where

import Data.Foldable
import Linear hiding (rotate)

parse = map toAction . lines
  where
    toAction (c : num) = toDir c (read num)
    toDir 'N' = Move (V2 0 1)
    toDir 'S' = Move (V2 0 (-1))
    toDir 'E' = Move (V2 1 0)
    toDir 'W' = Move (V2 (-1) 0)
    toDir 'L' = Rotate . degreeToTurns
    toDir 'R' = Rotate . (4 -) . degreeToTurns
    toDir 'F' = Forward
    toDir _ = error ""

    degreeToTurns n = if mod n 90 == 0 then div n 90 else error "not 90 degrees"

data Action = Forward Int | Rotate Int | Move (V2 Int) Int deriving (Show)

data Dir = N | S | E | W

manhattan :: V2 Int -> Int
manhattan (V2 x y) = abs x + abs y

rotate curVect n = iterate perp curVect !! n

solve1 = manhattan . fst . foldl' move (V2 0 0, V2 1 0)

move (curPos, curVect) action = case action of
  Forward i -> (curPos + (curVect * V2 i i), curVect)
  Move v i -> (curPos + (v * V2 i i), curVect)
  Rotate n -> (curPos, rotate curVect n)

solve2 = manhattan . fst . foldl' moveWaypoint (V2 0 0, V2 10 1)

moveWaypoint (curPos, waypoint) action = case action of
  Forward i -> (curPos + (waypoint * V2 i i), waypoint)
  Move v i -> (curPos, waypoint + (v * V2 i i))
  Rotate n -> (curPos, rotate waypoint n)

run xs = do
  let parsed = parse xs
  print $ solve1 parsed -- 319
  print $ solve2 parsed -- 50157
