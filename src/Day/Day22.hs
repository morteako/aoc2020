{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Day.Day22 where

import Control.Lens hiding (Empty, (:<), (|>))
import Data.Function.Memoize
import Data.List.Split
import qualified Data.Map.Strict as Map
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

solve1 = sum . fmap (uncurry (*)) . Seq.mapWithIndex ((,) . succ) . Seq.reverse . play

solve2 = sum . fmap (uncurry (*)) . Seq.mapWithIndex ((,) . succ) . Seq.reverse . either id id . playRec

type State = (Seq Int, Seq Int, Set.Set (Seq Int, Seq Int))

-- type MemMap = Map.Map State Res
-- type Res = Either (MemMap, Seq Int) (Seq Int, MemMap)

-- playRec :: (Seq Int, Seq Int) -> Either (Map.Map _1 _2, Seq Int) (Seq Int, Map.Map State (Either (Map.Map _1 _2, Seq Int) (Seq Int)))
playRec (a, b) = go Set.empty a b
  where
    go :: Set.Set (Seq Int, Seq Int) -> Seq Int -> Seq Int -> Either (Seq Int) (Seq Int)
    go prev xs ys
      | Set.member (xs, ys) prev =
        Left xs
    go prev xs'@(x :<| xs) ys'@(y :<| ys)
      | length xs >= x && length ys >= y,
        res <- go prev (Seq.take x xs) (Seq.take y ys) =
        case res of
          Left _ -> go prev' (xs |> x |> y) ys
          Right _ -> go prev' xs (ys |> y |> x)
      | x > y = go prev' (xs |> x |> y) ys
      | x < y = go prev' xs (ys |> y |> x)
      where
        prev' = Set.insert (xs', ys') prev
    go prev xs Empty = Left xs
    go prev Empty ys = Right ys

run xs = do
  let parsed = parse xs
  print $ solve1 parsed
  print $ solve2 parsed
