module Lib
  ( waitTime
  , isSolution
  , findT
  , findTFrom
  , findTCrt
  ) where

import Data.Maybe (catMaybes)
import Debug.Trace

waitTime :: Integer -> [Integer] -> (Integer, Integer)
waitTime arrival busses = bus
  where
    bus = minimum $ zip waitTimes busses
    wait sz bz = (div sz bz + 1) * bz - sz
    waitTimes = wait arrival <$> busses

increments :: [Maybe Integer] -> Integer -> [(Integer, Integer)]
increments [] _ = []
increments (x:xs) n =
  case x of
    Just i -> (i, n) : increments xs (n + 1)
    Nothing -> increments xs (n + 1)

isSolution :: Integer -> [(Integer, Integer)] -> Bool
isSolution t [] = True
isSolution t ((x, inc):xs) = rem (t + inc) x == 0 && isSolution t xs

trySolution :: [Maybe Integer] -> [(Integer, Integer)] -> [Integer] -> Integer
trySolution _ _ [] = 0
trySolution bs incs (x:xs) =
  if isSolution x incs
    then x
    else trySolution bs incs xs

findTFrom :: [Maybe Integer] -> Integer -> Integer
findTFrom bs s = trySolution bs incs solutions
  where
    incs = increments bs 0
    (inc, n) = maximum incs
    start = (div s inc + 1) * inc - n
    solutions = [start + inc * x | x <- [0 ..]]

findT :: [Maybe Integer] -> Integer
findT bs = findTFrom bs 0

crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (1, 0)
  where
    go (m1, r1) (m2, r2) = (m, r `mod` m)
      where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1
    -- Modular Inverse
    a `inv` m =
      let (_, i, _) = gcd a m
       in i `mod` m
    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
      where
        (g, s, t) = gcd (b `mod` a) a

findTCrt :: [Maybe Integer] -> Integer
findTCrt mbs = (m - r) `mod` r
  where
    (m, r) = crt $ increments mbs 0
