module Main where

import Data.HashMap as Map
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Lib
import System.Environment

tuplify2 :: [a] -> (a, a)
tuplify2 (x:y:xs) = (x, y)

countTrue :: [Bool] -> Int
countTrue ls =
  foldl
    (\acc p ->
       case p of
         True -> acc + 1
         False -> acc)
    0
    ls

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let splitInput = splitOn "\n\n" input
  let passports =
        fmap Map.fromList $
        fmap (fmap (tuplify2 . splitOn ":")) $ fmap words splitInput
    -- part 1
  let requiredFields =
        Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  let valid = countTrue $ fmap validPass passports
  putStrLn $ "part one answer: " ++ show valid
  -- part 2
  let valid2 = countTrue $ fmap validPass2 passports
  putStrLn $ "part two answer: " ++ show valid2
