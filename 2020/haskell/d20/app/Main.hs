module Main where

import Lib
import System.Environment
import Data.List.Split (splitOn)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromJust)

main :: IO ()
main = do
    args <- getArgs
    let path = head args
    input <- readFile path
    let tiles = toTile <$> splitOn "\n\n" input
    let arrangement = head $ solveArrangement . toArrangement $ tiles
    let part1answer = arrangementValue arrangement
    putStrLn $ "answer part1: " ++ show part1answer
    let picture = assamble arrangement
    let monsters = findAllMonsters picture
    let pc = countPixels picture
    let mc = countPixels monsters
    putStrLn $ "answer part2: " ++ show $ pc - mc
