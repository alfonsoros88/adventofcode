module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let inputFile = head args
    input <- readFile inputFile
    print $ part1 input
    print $ part2 input

