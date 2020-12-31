module Lib
  ( Pre
  , preamble
  , findInvalid
  , contSumMinMax
  ) where

import qualified Data.Dequeue as Q
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Debug.Trace

type Queue = Q.BankersDequeue Int

data Pre =
  Pre Int Queue (S.Set Int)
  deriving (Show)

preamble :: Int -> Pre
preamble n = Pre n Q.empty S.empty

full :: Pre -> Bool
full (Pre n q _) = length q == n

add :: Pre -> Int -> Pre
add p@(Pre n q s) x
  | full p = Pre n newQ newS
  | otherwise = Pre n (Q.pushBack q x) (S.insert x s)
  where
    (old, tmp) = fromJust $ Q.popFront q
    newQ = Q.pushBack tmp x
    newS = S.insert x (S.delete old s)

twoSum :: S.Set Int -> Int -> Bool
twoSum s x = any (\y -> S.member (x - y) s) s

validate :: Pre -> Int -> Bool
validate p@(Pre n _ s) x
  | not (full p) = True
  | otherwise = twoSum s x

findInvalid :: Pre -> [Int] -> Int
findInvalid _ [] = 0
findInvalid p (x:xs) =
  case validate p x of
    True -> findInvalid (add p x) xs
    False -> x

dropTillLess :: Queue -> Int -> Int -> (Queue, Int)
dropTillLess q s n =
  case Q.popFront q of
    Nothing -> (q, 0)
    Just (x, newQ) ->
      let y = s - x
       in if y <= n
            then (newQ, y)
            else dropTillLess newQ y n

contSum :: Queue -> Int -> [Int] -> Int -> Queue
contSum q _ [] _ = q
contSum q s (x:xs) n
  | s == n = q
  | y < n = contSum newQ y xs n
  | y > n = contSum decQ newSum xs n
  where
    y = x + s
    newQ = Q.pushBack q x
    (decQ, newSum) = dropTillLess newQ y n

contSumMinMax :: [Int] -> Int -> Int
contSumMinMax ls n = s + l
  where
    q = contSum Q.empty 0 ls n
    s = foldl1 min q
    l = foldl1 max q
