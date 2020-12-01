module Lib
  ( sumUpTo
  , tripletSum
  ) where

import Data.List (find)

sumUpTo :: [Int] -> Int -> Maybe (Int, Int)
sumUpTo [] _ = Nothing
sumUpTo (x:xs) n =
  case find ((n - x) ==) xs of
    Just y -> Just (x, y)
    Nothing -> sumUpTo xs n

tripletSum :: [Int] -> Int -> (Int, Int, Int)
tripletSum (x:xs) n =
  case sumUpTo (x : xs) (n - x) of
    Just (y, z) -> (x, y, z)
    Nothing -> tripletSum xs n
