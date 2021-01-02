module Main where

import Data.List
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let numbers = sort $ read <$> lines input :: [Int]
  let (ones, threes) = count1sAnd3sDiffs numbers
  putStrLn $ "answer part1: " ++ show (ones * threes)
  putStrLn $ "answer part2: " ++ show (countAlternatives numbers)
  putStrLn $ "answer part2: " ++ show (countAlternativesDP numbers)
