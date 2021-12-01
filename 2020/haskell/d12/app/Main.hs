module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let path = head args
    input <- readFile path
    let cmds = readCmd <$> lines input
    let part1Ship = foldl step newShip cmds
    putStrLn $ "answer part1: " ++ show (manhattan part1Ship)
    let (part2Ship, _) = foldl step2 (newShip, newWaypoint) cmds
    putStrLn $ "answer part2: " ++ show (manhattan part2Ship)
