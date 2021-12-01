module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let results = (eval . tokenize) <$> lines input
  putStrLn $ "anser part1: " ++ show (sum results)
  let res = (eval . resolveSum . tokenize) <$> lines input
  putStrLn $ "anser part2: " ++ show (sum res)
