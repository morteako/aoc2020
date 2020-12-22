module Day.Day22 where

import Control.Lens hiding (Empty, (:<), (|>))
import Data.List.Split
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Debug.Trace

parse = toTuple . fmap (Seq.fromList . map (read @Int) . tail . lines) . splitOn "\n\n"
  where
    toTuple [a, b] = (a, b)

play (a, b) = go a b
  where
    go (x :<| xs) (y :<| ys)
      | x > y = go (xs |> x |> y) ys
      | x < y = go xs (ys |> y |> x)
    go xs Empty = xs
    go Empty ys = ys

solve = sum . fmap (uncurry (*)) . Seq.mapWithIndex ((,) . succ) . Seq.reverse . play

solve2 = sum . fmap (uncurry (*)) . Seq.mapWithIndex ((,) . succ) . Seq.reverse . either id id . playRec

playRec (a, b) = go Set.empty a b
  where
    go prev xs ys
      -- | traceShow (xs, ys) False = undefined
      | Set.member (xs, ys) prev =
        Left xs
    go prev (x :<| xs) (y :<| ys)
      | length xs >= x && length ys >= y = case go Set.empty (Seq.take x xs) (Seq.take y ys) of
        Left _ -> go prev' (xs |> x |> y) ys
        Right _ -> go prev' xs (ys |> y |> x)
      | x > y = go prev' (xs |> x |> y) ys
      | x < y = go prev' xs (ys |> y |> x)
      where
        prev' = Set.insert (xs, ys) prev
    go prev xs Empty = Left xs
    go prev Empty ys = Right ys

run xs = do
  print xs
  let parsed = parse xs
  -- print parsed
  -- print $ solve parsed

  print $ solve2 parsed

-- print $ itoListOf (reversed . ifolded . reindexed (+ 1)) (play parsed)
-- print $ itoListOf (reversed . ifolded . reindexed (+ 1)) (play parsed)