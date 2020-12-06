module Day.Day06 where

import Data.List.Split (splitOn)
import qualified Data.Set as Set

parse :: String -> [[String]]
parse = fmap lines . splitOn "\n\n"

solve :: [[String]] -> Int
solve = sum . fmap (Set.size . Set.fromList . concat)

solve2 :: [[String]] -> Int
solve2 = sum . fmap (Set.size . foldr1 Set.intersection . fmap Set.fromList)

run :: String -> IO ()
run xs = do
  let parsed = parse xs
  print $ solve parsed -- 6596
  print $ solve2 parsed -- 3219