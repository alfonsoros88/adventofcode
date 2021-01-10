module Main where

import Lib

main :: IO ()
main = do
  let test = memGame [0, 3, 6] 2020
  let part1 = memGame [1, 2, 16, 19, 18, 0] 2020
  let part2 = memGame [1, 2, 16, 19, 18, 0] 30000000
  putStrLn $ "test part1: " ++ show test
  putStrLn $ "answer part1: " ++ show part1
  putStrLn $ "answer part2: " ++ show part2
