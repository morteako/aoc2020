module Day.Day13 where

import Control.Lens
import Control.Monad
import Data.Foldable
import qualified Data.IntSet as Set
import Data.List
import Data.List.Extra (maximumOn)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Ord
import Data.Semigroup
import Debug.Trace
import Text.Read (readMaybe)

parse (lines -> idLine : notes : _) = (read idLine, map readMaybe $ splitOn "," notes)

parse :: String -> (Integer, [Maybe Integer])

parse2 (lines -> notes : _) = map (readMaybe @Integer) $ splitOn "," notes

-- solve1 (id, busses) = subtract id $ fst $ minimumOn snd $ map (head . dropWhile (id >=) . ite) busses
--   where
--     ite x = iterate (+ x) x

-- solve1 :: (Int ,[Int]) -> (Int, [Int])
solve1 (id, catMaybes -> busses) = busId * (time - id)
  where
    (time, busId) = minimum $ do
      bus <- busses
      let m = head . dropWhile (id >=) $ iterate (+ bus) bus
      pure (m, bus)

-- lim = 5000

pattern T s <- (traceShowId -> s)

-- solve2 :: [Maybe Integer] -> Integer
solve2 maybeBusses = solveFast $ over (traverse . _1) toInteger $ itoListOf (folded . _Just) maybeBusses
  where
    r = over (traverse . _1) toInteger $ itoListOf (folded . _Just) maybeBusses

solve2Old maybeBusses = solvePaired $ over (traverse . _1) toInteger $ itoListOf (folded . _Just) maybeBusses

-- solveTest :: [Maybe Integer] -> (Integer, Integer, Integer)
-- solveTest maybeBusses = (a, b, res)
--   where
--     [a, b] = fmap snd pp
--     res = head $ filter check range
--     check t = all (\(ind, bus) -> mod (t + ind) bus == 0) pp
--     range = [0, minBus ..]

--     pp :: [(Integer, Integer)]
--     pp = over (traverse . _1) toInteger paired

--     paired :: [(Int, Integer)]
--     paired = itoListOf (folded . _Just) maybeBusses

--     (0, minBus) = head pp

-- test a b = do
--   print $ "Current " <> show (a, b)
--   print $ "prod : " <> show (a * b)
--   let res = map (so . f) [0 .. fromIntegral $ max a b]
--   print $ "MAXIMUM " <> show (maximumOn snd res)
--   mapM_ printNice res
--   where
--     printNice (n, (a, b, res)) = putStrLn $ "x-" <> show n <> " -> " <> show res <> "\t: " <> show (div res a)
--     so n = (length n - 1, solveTest n)
--     f n = [Just a] ++ replicate n Nothing ++ [Just b]

-- calc a b delta = (abDiv, sub, p1 - sub)
--   where
--     p1 = a * abDiv ^ mod (delta - 1) a
--     sub = div (delta - 1) a
--     -- abDiff = abs (b - a)
--     abDiv = div b a

-- solvePaired :: [(Integer, Integer)] -> Integer
-- solvePaired busses = res
--   where
--     res = head $ filter check range
--     check t = all (\(ind, bus) -> mod (t + ind) bus == 0) busses
--     range = [minI, minBus + minI ..]

--     (minI, minBus) = head busses

solvePaired :: [(Integer, Integer)] -> Integer
solvePaired busses = res
  where
    res = head $ filter check range
    check t = all (\(ind, bus) -> mod (t + ind) bus == 0) busses
    range = [0, minBus ..]

    (0, minBus) = head busses

-- solveFast :: [(Integer, Integer)] -> (Integer, Integer)
solveFast (T busses) = chinese_ind_mods inds moods - maxInds
  where
    (inds, moods) = unzip $ map fixInd busses
    fixInd (i, x) = (abs (i - maxInds) `mod` x, x)
    maxInds = maximum $ map fst busses

-- f (curPos,a) (delta, b) = solve2 [Just a,

-- solveTwo :: Integer -> Integer -> Integer -> Integer
-- solveTwo a b delta = head $ filter check [0, b ..]
--   where
--     check t = mod (t + delta) a == 0

chinese_ind_mods inds mods = flip mod nProd $ sum $ zipWith f inds mods
  where
    f a1 ni = let bi = div nProd ni in a1 * bi * modInv bi ni

    nProd = product mods

modInv :: Integer -> Integer -> Integer
modInv a m
  | 1 == g = mkPos i
  | otherwise = error "wtf"
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Integer -> Integer -> (Integer, Integer, Integer)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
   in (t, s - q * t, g)

run xs = do
  let parsed = parse xs

  -- print parsed
  -- print $ solve1 parsed

  print $ solve2 $ snd parsed

-- test 2 5

-- test 2 11
-- test 2 13
-- test 2 17

-- forM_ [1 .. 7] $ \i -> do
--   -- print i
--   print $ calc 2 5 i

-- test 2 7

-- forM_ [1 .. 7] $ \i -> do
--   print $ calc 2 7 i

-- test 3 7

-- forM_ [1 .. 7] $ \i -> do
--   print $ calc 3 7 i

-- test 11 3

-- forM_ [1 .. 7] $ \i -> do
--   print $ calc 11 3 i

-- test 3 11

-- forM_ [1 .. 7] $ \i -> do
--   print $ calc 3 11 i

-- test 11 5

-- forM_ [1 .. 7] $ \i -> do
--   print $ calc 11 5 i

-- -- let t2 = parse2 "67,7,59,61"
-- let t2 = parse2 "17,x,13,19"
-- let t2 = parse2 "2,5,11"

-- print $ solve2 t2

-- print $ solve2 $ parse2 "67,7,x,59,61"

-- print $ solve2Old t2

-- print $ chinese_ind_mods [0, 1, 0] [2, 5, 11]

-- print $ chinese_ind_mods [2, 0, 3] [13, 17, 19]
-- print $ chinese_ind_mods [2, 19 -17, 0] [13, 17, 19]

-- print $ map (mod 782) [13, 17, 19]

-- print $ chinese_ind_mods [0, 1] [2, 3]
-- print $ chinese_ind_mods [0, 1] [2, 5]
-- print $ chinese_ind_mods [1, 0] [3, 11]
-- print $ chinese_ind_mods [2, 1, 0] [3, 5, 11]

-- print $ solveTwo 11 5 1
-- print $ solveTwo 11 5 2
-- print $ solveTwo 11 5 3

-- print $ solveFast t2

-- print $ calc 3 11 1
-- print $ calc 11 3 1
