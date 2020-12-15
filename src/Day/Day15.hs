module Day.Day15 where

import qualified Data.IntMap as Map
import Data.List.Split (splitOn)

parse :: String -> [Int]
parse = map read . splitOn "," . head . lines

solve1 lim xs = go (Map.fromList $ zip xs [1 ..]) (last xs) (length xs + 1)
  where
    go cur prev i
      | i > lim = prev
      | Just prevprev <- Map.lookup prev cur =
        go (Map.insert prev (i - 1) cur) (i - 1 - prevprev) (i + 1)
      | otherwise = go (Map.insert prev (i - 1) cur) 0 (i + 1)

run xs = do
  let parsed = parse xs

  print parsed

  print $ solve1 30000000 parsed
  pure ()
