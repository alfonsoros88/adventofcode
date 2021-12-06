module Main where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.List.Split (splitOn)

data Line =
  Line
    { begin :: (Int, Int)
    , end :: (Int, Int)
    }
  deriving (Show, Eq)

-- Parse a line from a string of the form "x1,y1 -> x2,y2"
parseLine :: String -> Line
parseLine s =
  let (p1:p2:_) = splitOn " -> " s
      (x1:y1:_) = map read $ splitOn "," p1
      (x2:y2:_) = map read $ splitOn "," p2
   in Line (x1, y1) (x2, y2)

-- Return a list of points in the line
points :: Line -> [(Int, Int)]
points l@(Line (x1, y1) (x2, y2))
    | isDiagonal l = zip [x1, x1 + xStep .. x2] [y1, y1 + yStep .. y2]
    | otherwise = [(x, y) | x <- xs, y <- ys]
    where
        xs = if x1 < x2 then [x1 .. x2]  else [x1, x1 - 1 .. x2]
        ys = if y1 < y2 then [y1 .. y2]  else [y1, y1 - 1 .. y2]
        xStep = if x1 < x2 then 1 else -1
        yStep = if y1 < y2 then 1 else -1

-- Return true if the line is diagonal
isDiagonal :: Line -> Bool
isDiagonal (Line (x1, y1) (x2, y2)) = x1 /= x2 && y1 /= y2

countPoints :: [Line] -> HashMap (Int, Int) Int
countPoints lines =
  foldl (\m p -> Map.insertWith (+) p 1 m) Map.empty $ concatMap points lines

part1 input =
  let pointsCount =
        countPoints . filter (not . isDiagonal) . map parseLine . lines $ input
   in Map.size . Map.filter (>= 2) $ pointsCount

-- part2

part2 input =
  let pointsCount = countPoints . map parseLine . lines $ input
   in Map.size . Map.filter (>= 2) $ pointsCount

main :: IO ()
main = do
  input <- readFile "inputs/d05.txt"
  print $ part1 input
  print $ part2 input
