module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let path = head args
    input <- readFile path
    let matrix =  readInput input
    let matrix4d = [matrix]
    let part1 = applyNTimes 6 computeNextGen matrix
    let part2 = applyNTimes 6 computeNextGen4d matrix4d
    putStrLn $ "part1 answer: " ++ show (countActive part1)
    putStrLn $ "part2 answer: " ++ show (countActive4d part2)
