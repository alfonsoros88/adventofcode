module Main where

import Lib
import System.Environment (getArgs)
import qualified Data.Map as Map

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let entries = parseEntries input
  let part1 = countHowManyCanHold entries "shiny gold"
  putStrLn $ "answer part1: " ++ show part1
  let part2 = countBagsInside entries "shiny gold"
  putStrLn $ "answer part2: " ++ show part2
