module Main where
import Data.List.Split (splitOn)
import Control.Monad (guard)

type Fish = [Int]

parseFishList :: String -> Fish
parseFishList = map read . splitOn ","

nextDay :: Fish -> Fish
nextDay [] = []
nextDay (x:xs) =
  case x of
    0 -> 6 : 8 : nextDay xs
    _ -> (x - 1) : nextDay xs

nthDay :: Int -> Fish -> Fish
nthDay 0 fish = fish
nthDay n fish = nthDay (n - 1) (nextDay fish)

part1 = length . nthDay 80 . parseFishList

part2 = length . nthDay 256 . parseFishList

main :: IO ()
main = do
  input <- readFile "inputs/d06.txt"
  print $ part1 input
  -- print $ part2 input
