module Main where

-- Reads each line of the file and returns a list of numbers
readNumbers :: String -> [Int]
readNumbers = map read . lines

-- Returns the difference between each adjacent pair of numbers in the input
-- list
-- e.g. [1,2,3,4] -> [1, 1, 1]
--      [3, 5, 8, 13] -> [2, 3, 5]
differences :: [Int] -> [Int]
differences (x:y:xs) = (y - x) : differences (y:xs)
differences _ = []

-- Count the number of positive numbers in the list
countPositives :: [Int] -> Int
countPositives = length . filter (> 0)

-- Takes the list of depth values, takes the differences between of adjacent
-- values and returns the number of positive differences
part1 :: String -> Int
part1 = countPositives . differences . readNumbers

-- part2

-- Returns sub-arrays of the input list of length n
windows :: Int -> [Int] -> [[Int]]
windows n xs
    | length xs < n = []
    | otherwise = take n xs : windows n (tail xs)

-- Do the same as part1 but in windows of size 3
part2 = countPositives . differences . fmap sum . windows 3 . readNumbers

main :: IO ()
main = do
    input <- readFile "inputs/d01.txt"
    print $ part1 input
    print $ part2 input
