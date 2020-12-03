module Day.Day03 where

import Control.Monad ((>=>))
import Data.Functor.Foldable
import Data.Maybe (fromMaybe)
import Safe (headMay)

-- walk :: Int -> Int -> [[a]] -> [a]
walk :: Int -> Int -> [String] -> Int
walk right down = hylo len move
  where
    move [] = Nil
    move xs = fromMaybe Nil $ do
      let movedRight = fmap (drop right) xs
      let movedDown = drop down movedRight
      a <- (headMay >=> headMay) movedDown
      pure $ Cons a movedDown

    len Nil = 0
    len (Cons '#' x) = x
    len (Cons _ x) = x

solve1 :: [String] -> Int
solve1 = walk 3 1

solve2 :: [String] -> Int
solve2 xs = hylo prod build [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  where
    build [] = Nil
    build ((n, m) : ys) = Cons (walk n m xs) ys

    prod Nil = 1
    prod (Cons x xs) = x * xs

parse :: String -> [String]
parse = fmap cycle . lines

run :: String -> IO ()
run (parse -> parsed) = do
  print $ solve1 parsed -- 225
  print $ solve2 parsed -- 1115775000
