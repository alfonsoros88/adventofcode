module Main where

import Data.List (sort)
import Data.List.Split (splitOn)

type Positions = [Int]

readPositions :: String -> Positions
readPositions = map read . splitOn ","

median :: [Int] -> Int
median xs =
  let sorted = sort xs
      len = length sorted
   in if odd len
        then sorted !! (len `div` 2)
        else (sorted !! (len `div` 2 - 1) + sorted !! (len `div` 2)) `div` 2

fuelCost :: Positions -> Int -> Int
fuelCost positions target = foldl (\acc x -> acc + abs (x - target)) 0 positions

part1 input = fuelCost positions target
  where
    positions = readPositions input
    target = median positions

-- part2

fuelCostAdjusted :: Positions -> Int -> Int
fuelCostAdjusted positions target =
  foldl (\acc x -> acc + stepCost (abs (x - target))) 0 positions

stepCost n = n * (n + 1) `div` 2

posibleTargets :: Positions -> [Int]
posibleTargets positions = [minBound .. maxBound]
    where
        minBound = minimum positions
        maxBound = maximum positions

findBestTarget :: Positions -> Int
findBestTarget positions =
    let targets = posibleTargets positions
        costs = fuelCostAdjusted positions <$> targets
     in snd $ foldr1 (\p1@(c1, _) p2@(c2, _) -> if c1 < c2 then p1 else p2) $ zip costs targets

part2 input = fuelCostAdjusted positions target
  where
    positions = readPositions input
    target = findBestTarget positions

main :: IO ()
main = do
  input <- readFile "inputs/d07.txt"
  print $ part1 input
  print $ part2 input
