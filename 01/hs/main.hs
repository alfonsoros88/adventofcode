import System.Environment (getArgs)
import System.IO (readFile)
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List (sortBy)

splitByNothing :: [Maybe a] -> [[a]]
splitByNothing maybes = group maybes [] []
    where
        group :: [Maybe a] -> [a] -> [[a]] -> [[a]]
        group [] current_list total = current_list : total
        group ((Just x):rest) current_list total = group rest (x:current_list) total
        group (Nothing:rest) current_list total = group rest [] (current_list:total)

readNumberGroups :: String -> [[Int]]
readNumberGroups = splitByNothing . map readMaybe . lines

part1 :: String -> Int
part1 = maximum . sumGroups . readNumberGroups

part2 :: String -> Int
part2 = sum . take 3 . sortBy (flip compare) . sumGroups . readNumberGroups

sumGroups :: [[Int]] -> [Int]
sumGroups = fmap sum

main :: IO ()
main = do
    args <- getArgs
    let input = head args
    input <- readFile input
    print $ part1 input
    print $ part2 input
