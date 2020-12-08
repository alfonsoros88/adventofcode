module Main where

import Lib
import System.Environment
import Data.Maybe
import Data.List

main :: IO ()
main = do
    args <- getArgs
    let path = head args
    input <- readFile path
    let ids = sort $ catMaybes $ fmap seatId $ lines input
    putStrLn $ "part one answer: " ++ show (last ids)
    -- part 2
    putStrLn $ "part two answer: " ++ show (findMissing ids)

findMissing :: [Int] -> Int
findMissing [] = 0
findMissing (x:y:xs)
    | y - x > 1 = x + 1
    | otherwise = findMissing (y:xs)
