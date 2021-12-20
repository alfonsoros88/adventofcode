module Main where

import Data.Bifunctor (first, second)
import Data.List (nub, partition, sort)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

type Point = (Int, Int)

data Fold
  = X Int
  | Y Int
  deriving (Show, Eq)

parsePointsAndFolds input =
  let (points:folds:_) = splitOn "\n\n" input
   in (parsePoints points, parseFolds folds)

toPoint :: [Int] -> Point
toPoint [x, y] = (x, y)
toPoint _ = error "toPoint: invalid input"

parsePoints :: String -> [Point]
parsePoints = map (toPoint . map read . splitOn ",") . lines

parseFolds :: String -> [Fold]
parseFolds = map (dir . splitOn "=" . (!! 2) . words) . lines
  where
    dir (d:x:_) =
      case d of
        "y" -> Y (read x)
        "x" -> X (read x)
        _ -> error "parseFolds: invalid direction"
    dir _ = error "parseFolds: invalid input"

axisSize :: [Point] -> (Int, Int)
axisSize points = (maxX - minX, maxY - minY)
    where
        minX = minimum $ map fst points
        maxX = maximum $ map fst points
        minY = minimum $ map snd points
        maxY = maximum $ map snd points

foldPoints (Y y) points =
  let (same, toModify) = partition ((< y) . snd) points
   in nub $ same ++ map (second ((2 * y) -)) toModify
foldPoints (X x) points =
  let (same, toModify) = partition ((< x) . fst) points
   in nub $ same ++ map (first ((2 * x) -)) toModify

part1 input =
  let (points, folds) = parsePointsAndFolds input
   in length $ foldr foldPoints points (take 1 folds)

--------------------------------------------------------------------------------
-- Part 2

foldAll :: String -> [Point]
foldAll input =
  let (points, folds) = parsePointsAndFolds input
   in sort $ foldl (flip foldPoints) points folds

displayCode :: [Point] -> String
displayCode points =
  let (minX, maxX) = (minimum $ map fst points, maximum $ map fst points)
      (minY, maxY) = (minimum $ map snd points, maximum $ map snd points)
      xs = [minX .. maxX]
      ys = [minY .. maxY]
   in unlines $ map (\y -> map (\x -> if (x, y) `elem` points then '#' else '.') xs) ys

part2 = displayCode . foldAll

main :: IO ()
main = do
  input <- readFile "inputs/d13.txt"
  print $ part1 input
  putStrLn $ part2 input
