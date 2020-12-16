module Day.Day16 where

import Control.Lens
import Control.Monad
import qualified Data.IntSet as Set
import Data.List
import Data.List (transpose)
import Data.List.Split
import Debug.Trace

readInt = read @Int

type IntPair = (Int, Int)

type S = ([(String, [IntPair])], [Int], [[Int]])

parse xs = case splitOn "\n\n" $ xs of
  [ranges, myTicket, nearbyTickets] -> (foldMap parseProp $ lines ranges, parseTickets nearbyTickets)
  where
    parseProp (splitOn ": " -> [name, splitOn " or " -> [r1, r2]]) = [parseRange r1, parseRange r2]
      where
        parseRange (splitOn "-" -> [n1, n2]) = (readInt n1, readInt n2)

    parseTickets = fmap (fmap readInt . splitOn ",") . tail . lines

parse2 :: [Char] -> S
parse2 xs = case splitOn "\n\n" $ xs of
  [ranges, traceShowId . head . drop 2 . words -> myTicket, nearbyTickets] -> (map parseProp $ lines ranges, readInt <$> splitOn "," myTicket, parseTickets nearbyTickets)
  where
    parseProp (splitOn ": " -> [name, splitOn " or " -> [r1, r2]]) = (name, [parseRange r1, parseRange r2])
      where
        parseRange (splitOn "-" -> [n1, n2]) = (readInt n1, readInt n2)

    parseTickets = fmap (fmap readInt . splitOn ",") . tail . lines

inRange x (a, b) = a <= x && x <= b

solve1 (ranges, tickets) = sum $ foldMap (filter (\t -> not $ checkInSomeRange t ranges)) tickets

checkInSomeRange x ranges = any (inRange x) ranges

checkInRangeGroup x rangeGroup = any (inRange x) rangeGroup

solve2 :: S -> Integer
solve2 (ranges, yourTicket, tickets) =
  let r = getColumns (ranges, tickets)
      cols = map (Set.findMin . snd) $ take 6 $ sortOn fst $ findComb r
   in product $ map toInteger $ map (yourTicket !!) cols

solve2' (ranges, yourTicket, tickets) =
  let r = getColumns (ranges, tickets)
      cols = map (Set.findMin . snd) $ take 6 $ sortOn fst $ findComb r
   in findComb r

filterTickets ranges tickets = filter (\tick -> all (checkInSomeRange tick) $ concat ranges) tickets

getColumns (ranges, tickets) = do
  let filteredTickets = filter (all (flip checkInSomeRange (concatMap snd ranges))) tickets
  ts <- transpose filteredTickets
  pure $ Set.fromList $ map fst $ filter (\(_, r) -> all (\t -> checkInRangeGroup t r) ts) ranges

-- pure $ ts

findComb xs = if all Set.null xs then undefined else combine (zip [0 ..] xs)
  where
    combine :: [(Int, Set.IntSet)] -> [(Int, Set.IntSet)]
    combine xs =
      let (ones, rest) = partition ((1 ==) . Set.size . snd) xs
          ws = Set.unions $ fmap snd ones
       in if null rest then ones else ones ++ combine ((over (traverse . _2) (flip Set.difference ws)) rest)

run xs = do
  -- print xs
  let parsed = parse xs
  -- print $ parse2 xs

  -- print $ solve1 parsed
  -- let (ranges, tickets) = parse2 xs
  -- mapM print ranges
  -- mapM print tickets
  print $ solve2 $ parse2 xs
  print $ solve2' $ parse2 xs

  print $ (1086463551769 :: Int)

  -- mapM (\x -> print (length x) >> putStrLn "\n") $ transpose tickets
  -- print $ transpose (transpose tickets) == tickets

  pure ()