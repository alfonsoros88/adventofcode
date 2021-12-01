module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let prg = parsePrg input
  putStrLn $ "answer part1: " ++ show (run prg)
  putStrLn $ "answer part2: " ++ show (runCorrect prg)
