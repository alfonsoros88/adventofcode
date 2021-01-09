module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let instructions = parseInstruction <$> lines input
    -- part1
  let finalState = foldl step emptyState instructions
  let map = Map.fromList $ reverse $ mem finalState
  let part1 = sum $ Map.elems map
  putStrLn $ "answer part1: " ++ show part1
    -- part2
  let finalState2 = foldl step2 emptyState instructions
  let map2 = Map.fromList $ mem finalState2
  let part2 = sum $ Map.elems map2
  putStrLn $ "answer part2: " ++ show part2
