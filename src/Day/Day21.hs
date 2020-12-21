module Day.Day21 where

import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

parse = map (f . splitOn " (contains ") . lines
  where
    f [words -> ingrs, init -> allergens] =
      (Map.fromListWith (+) $ map (,1) ingrs, foldMap (\x -> Map.singleton x (Set.fromList ingrs)) $ splitOn ", " allergens)

solve1 (unzip -> (ingrs, allergenMap)) = sum $ Map.withoutKeys ingCounts allergenic
  where
    ingCounts = Map.unionsWith (+) ingrs
    allergenic = fold . Map.unionsWith Set.intersection $ allergenMap

solve2 (unzip -> (ingrs, allergenMap)) = concat $ intersperse "," $ map snd xs
  where
    xs = sort $ over (traverse . _2) Set.findMin $ findComb $ Map.toList allergenic
    allergenic = Map.unionsWith Set.intersection $ allergenMap

findComb :: [(String, Set.Set String)] -> [(String, Set.Set String)]
findComb xs = case rest of
  [] -> ones
  _ -> ones ++ findComb ((over (traverse . _2) removeOld) rest)
  where
    (ones, rest) = partition ((1 ==) . Set.size . snd) xs
    removeOld w = Set.difference w (Set.unions $ fmap snd ones)

run xs = do
  --   print xs
  let parsed = parse xs
  --   print parsed
  print $ solve1 parsed
  putStrLn $ solve2 parsed