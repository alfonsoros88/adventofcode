module Main where

import Data.List.Split
import qualified Data.Set as Set
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let groups = splitOn "\n\n" input
  let personsByGroup = fmap lines groups
  let part1 = countDifferentAnswers personsByGroup
  putStrLn $ "answer part one: " ++ show part1
    -- part 2
  let part2 = countCommonAnswers personsByGroup
  putStrLn $ "answer part one: " ++ show part2
