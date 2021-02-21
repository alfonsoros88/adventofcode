module Main where

import Data.List.Split
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let [rules, messages] = lines <$> splitOn "\n\n" input
  let parser = fromRules rules
  let matches = consume parser <$> messages
  let valid = length . filter ((Just "") ==) $ matches
  putStrLn $ "answer part1: " ++ show valid
  let matches2 = match parser <$> messages
  let valid2 = (length . filter id) matches2
  putStrLn $ "answer part1: " ++ show valid2
