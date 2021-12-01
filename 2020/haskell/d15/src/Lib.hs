module Lib
  ( memGame
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type Seed = [Int]

memGame :: Seed -> Int -> Int
memGame s 0 = 0
memGame s n
  | n <= t = s !! (n - 1)
  | otherwise = memGame_ m l t n
  where
    l = last s
    t = length s
    m = IntMap.fromList (zip s [1 .. t])

memGame_ :: IntMap Int -> Int -> Int -> Int -> Int
memGame_ m l t n
  | t == n = l
  | t < n = memGame_ m' l' (t + 1) n
  where
    l' =
      case IntMap.lookup l m of
        Just x ->
          if x == t
            then 0
            else t - x
        Nothing -> 0
    m' = IntMap.insert l t m
