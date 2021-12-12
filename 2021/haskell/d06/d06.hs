module Main where
import Data.List.Split (splitOn)
import Control.Monad (guard)
import Data.Vector (Vector)
import qualified Data.Vector as V

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

memo :: Int -> Int -> Int
memo x = (map (notFib x) [0..] !!)
    where
        notFib :: Int -> Int -> Int
        notFib _ 0 = 1
        notFib x n
            | x > 0 = memo (x - 1) (n - 1)
            | otherwise = memo 6 (n - 1) + memo 8 (n - 1)

nthDay' :: Int -> Fish -> Int
nthDay' n = sum . map (`memo` n)

part2 = nthDay' 256 . parseFishList

main :: IO ()
main = do
  input <- readFile "inputs/d06.txt"
  print $ part1 input
  print $ part2 input
