module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let board = fmap (map toCell) (lines input)
  let finalBoard1 = converge board cellStep
  let neighbours = computeCellNeighbours board
  let finalBoard2 = converge board (cellStep2 neighbours)
  putStrLn $ "answer part1: " ++ show (countOccupiedTotal finalBoard1)
  putStrLn $ "answer part2: " ++ show (countOccupiedTotal finalBoard2)
