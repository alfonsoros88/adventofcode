module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let path = head args
    input <- readFile path
    let numbers = read <$> lines input :: [Int]
    let pre = preamble 25
    let invalid = findInvalid pre numbers
    putStrLn $ "answer part1: " ++ show invalid
    let part2 = contSumMinMax numbers invalid
    putStrLn $ "answer part2: " ++ show part2
