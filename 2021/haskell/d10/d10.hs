module Main where
import Data.List (sort)

isClosing :: Char -> Bool
isClosing c = c == ')' || c == ']' || c == '}' || c == '>'

parse :: [Char] -> [Char] -> ([Char], [Char])
parse stack [] = (stack, [])
parse ('(':xs) (')':ys) = parse xs ys
parse ('[':xs) (']':ys) = parse xs ys
parse ('{':xs) ('}':ys) = parse xs ys
parse ('<':xs) ('>':ys) = parse xs ys
parse stack (y:ys)
    | isClosing y = (stack, y:ys)
    | otherwise = parse (y:stack) ys

points :: Char -> Int
points ')' = 3
points ']' = 57
points '}' = 1197
points '>' = 25137
points _ = error "Invalid character"

part1 = sum . map (points . head . snd) . filter (not . null . snd) . map (parse []) . lines

-- part 2

points' :: Char -> Int
points' '(' = 1
points' '[' = 2
points' '{' = 3
points' '<' = 4
points' _ = error "Invalid character"

computePoints :: [Char] -> Int
computePoints = foldl (\acc x -> 5 * acc + points' x) 0

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

part2 = middle . sort . map (computePoints . fst) . filter (null . snd) . map (parse []) . lines

main :: IO ()
main = do
  input <- readFile "inputs/d10.txt"
  print $ part1 input
  print $ part2 input
