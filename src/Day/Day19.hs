{-# LANGUAGE FlexibleContexts #-}

module Day.Day19 where

-- import Text.Parsec hiding (parse)
-- import qualified Text.Parsec as P

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Either
import qualified Data.IntMap.Strict as Map
import Data.List.Split
import qualified Data.Set as Set
import Debug.Trace
import qualified Text.ParserCombinators.ReadP as RP

parse xs = case fmap lines $ splitOn "\n\n" xs of
  [map f -> rules, msgs] -> (Map.fromList $ rules, msgs)
  where
    f (splitOn ": " -> [n, rule]) = (read @Int n, readRule rule)

    readRule str@('"' : _) = Single $ read str
    readRule (splitOn " | " -> rules) = case rules of
      [xs] -> Conc $ map read $ words xs
      _ -> Many $ map readRule $ rules

stringFromRule m (Single s) = [s]
stringFromRule m (Conc [x]) = (stringFromRule m . (m Map.!)) x
stringFromRule m (Conc (x : xs)) = (++) <$> (stringFromRule m . (m Map.!)) x <*> stringFromRule m (Conc xs)
stringFromRule m (Many xs) = foldMap (stringFromRule m) xs

solve1 (ruleMap, msgs) = Set.size $ Set.intersection msgSet allowed
  where
    msgSet = Set.fromList msgs
    allowed = Set.fromList $ stringFromRule ruleMap (ruleMap Map.! 0)

data Rule = Single String | Conc [Int] | Many [Rule] deriving (Show)

parserFromRule :: Map.IntMap Rule -> Rule -> RP.ReadP String
parserFromRule m (Single s) = RP.string s
parserFromRule m (Conc xs) = foldMap (parserFromRule m . (m Map.!)) xs
parserFromRule m (Many xs) = RP.choice (map (parserFromRule m) xs)

instance Semigroup a => Semigroup (RP.ReadP a) where
  a <> b = pure (<>) <*> a <*> b

instance Monoid a => Monoid (RP.ReadP a) where
  mempty = pure mempty

-- solve2 :: (Map.IntMap Rule, [String]) -> [Either ParseError String]
solve2 (fixRuleMap -> ruleMap, msgs) = filter (not . null) $ map f msgs
  where
    f s = filter (null . snd) $ RP.readP_to_S parser s

    parser = parserFromRule ruleMap (ruleMap Map.! 0)

fixRuleMap m = Map.insert 8 (Many [Conc [42], Conc [42, 8]]) $ Map.insert 11 (Many [Conc [42, 31], Conc [42, 11, 31]]) m

--     msgSet = Set.fromList msgs
--     allowed = Set.fromList $ stringFromRule ruleMap (ruleMap Map.! 0)

-- parseAstar :: RP.ReadP String
-- parseAstar = ((<>) <$> RP.string "s" <*> parseAstar) <|> RP.string "s"

-- parseAstar' :: RP.ReadP String
-- parseAstar' = RP.string "s" <|> ((<>) <$> RP.string "s" <*> parseAstar)

run xs = do
  -- print $ runParserT parseAstar () "" "ss"

  --   print $ runParserT (parseAstar <* eof) () "" "ss"

  -- print $ RP.readP_to_S parseAstar "ss"
  -- print $ RP.readP_to_S parseAstar "s"

  -- print $ RP.readP_to_S parseAstar' "ss"
  -- print $ RP.readP_to_S parseAstar' "s"

  -- print xs
  --   print $ parse xs
  print $ solve1 $ parse xs
  -- mapM_ print $ solve2 $ parse xs
  print $ solve2 $ parse xs
