module Main where

import Data.List.Split
import Data.Maybe (catMaybes)
import Lib
import System.Environment
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let firstLine:secondLine:_ = lines input
  let arrival = read firstLine :: Integer
  let busses = readMaybe <$> splitOn "," secondLine :: [Maybe Integer]
  let (time, bus) = waitTime arrival (catMaybes busses)
  putStrLn $ "answer part1: " ++ show (time * bus)
    -- let t = findTFrom busses 100000000000000
    -- let t = findT busses
  let t = findTCrt busses
  putStrLn $ "answer part2: " ++ show t
