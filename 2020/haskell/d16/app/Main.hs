module Main where

import Data.List (transpose)
import Data.List.Split
import Data.Maybe (catMaybes, isNothing)
import Data.String.Utils (startswith)
import Lib
import System.Environment

toIntList :: String -> [Int]
toIntList ls = read <$> splitOn "," ls

part2 :: [Field] -> [Int] -> Int
part2 fields ticket = product values
  where
    z = zip fields ticket
    departureFields = filter (startswith "departure" . name . fst) z
    values = snd <$> departureFields

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let parts = splitOn "\n\n" input
  let fields = fmap toField $ lines $ head parts
  let myTicket = toIntList $ (!! 1) $ lines $ parts !! 1
  let tickets = fmap toIntList $ tail $ lines $ parts !! 2
  let part1 = sum $ catMaybes $ notMatchingNumber fields <$> tickets
  putStrLn $ "answer part1: " ++ show part1
    -- part 2
  let valid = filter (isNothing . notMatchingNumber fields) tickets
  let matchs = (`matchingFields` fields) <$> transpose valid
  let fieldOrder = findSolution matchs
  let fieldsSorted = sortFields fieldOrder fields
  putStrLn $ "answer part2: " ++ show (part2 fieldsSorted myTicket)
