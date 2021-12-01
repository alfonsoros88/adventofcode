module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let readInt s = read s :: Int
  let forest = ForestTile $ lines input
  let partOneTreesCount = countTreesFromRoot forest (1, 3)
  putStrLn $ "part one answer: " ++ show partOneTreesCount

  -- part 2
  let slopes = [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
  let costFunc = countTreesFromRoot forest
  let score = foldr1 (*) $ fmap costFunc slopes
  putStrLn $ "part two answer: " ++ show score

