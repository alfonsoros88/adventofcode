{-# LANGUAGE TupleSections #-}

module Main where

import Data.Biapplicative (second)
import Data.Function.Memoize (memoFix3, memoize2, memoize3)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import qualified Data.List.Split as Split (splitOn)

splitOn d input =
  let (x:y:_) = Split.splitOn d input
   in (x, y)

type Rules = HashMap String Char

parseStateAndRules input =
  let (init, rules) = splitOn "\n\n" input
   in (init, parseRules rules)

parseRules :: String -> Rules
parseRules = Map.fromList . map (second head . splitOn " -> ") . lines

step :: Rules -> String -> String
step _ [] = []
step rules l@(x:xs) =
  let h = take 2 l
   in case Map.lookup h rules of
        Nothing -> x : step rules xs
        Just y -> x : y : step rules xs

applyN :: Int -> Rules -> String -> String
applyN n rules = foldr (.) id (replicate n (step rules))

countChars :: String -> HashMap Char Int
countChars = Map.fromListWith (+) . map (, 1)

score :: HashMap Char Int -> Int
score m =
  let occ = Map.elems m
   in maximum occ - minimum occ

part1 input =
  let (init, rules) = parseStateAndRules input
   in score . countChars $ applyN 10 rules init

--------------------------------------------------------------------------------
-- Part 2
stepRec ::
     Rules
  -> (Char -> Char -> Int -> HashMap Char Int)
  -> Char
  -> Char
  -> Int
  -> HashMap Char Int
stepRec _ _ a b 0 = Map.fromList [(a, 1)]
stepRec rules f a b n =
  case Map.lookup [a, b] rules of
    Nothing -> Map.fromList [(a, 1)]
    Just x -> Map.unionWith (+) (f a x (n - 1)) (f x b (n - 1))

stepMemo rules = memoFix3 (stepRec rules)

countChars' :: Rules -> Int -> String -> HashMap Char Int
countChars' _ _ [] = Map.empty
countChars' _ _ [x] = Map.fromList [(x, 1)]
countChars' rules n (x:y:xs) =
  Map.unionWith (+) (memo x y n) (countChars' rules n (y : xs))
  where
    memo = stepMemo rules

part2 input =
  let (init, rules) = parseStateAndRules input
      countMemo = memoize2 (countChars' rules)
   in score $ countMemo 40 init

main :: IO ()
main = do
  input <- readFile "inputs/d14.txt"
  print $ part1 input
  print $ part2 input
