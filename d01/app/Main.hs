module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let readInt s = read s :: Int
  let numbers = map readInt . lines $ input
  let Just (x, y) = sumUpTo numbers 2020
  putStrLn $ "part one answer: " ++ show (x * y)
  let (u, v, w) = tripletSum numbers 2020
  putStrLn $ "part two answer: " ++ show (u * v * w)
