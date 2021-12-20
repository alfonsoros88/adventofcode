{-# LANGUAGE TupleSections #-}

module Main where

import Data.Char (isUpper)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List.Split (splitOn)

type G = HashMap String [String]

bothWays :: [String] -> [(String, [String])]
bothWays (x:y:_) = [(x, [y]), (y, [x])]
bothWays _ = error "tuplify2: not enough elements"

parseGraph :: String -> G
parseGraph = M.fromListWith (++) . concatMap (bothWays . splitOn "-") . lines

isBigCave :: String -> Bool
isBigCave = all isUpper

countPaths :: String -> String -> G -> Int
countPaths s e g = dfs' g S.empty s
  where
    dfs' g visited x
      | x == e = 1
      | not (isBigCave x) && x `S.member` visited = 0
      | otherwise =
        sum $ map (dfs' g (x `S.insert` visited)) (M.lookupDefault [] x g)

part1 = countPaths "start" "end" . parseGraph

--------------------------------------------------------------------------------
-- Part 2

countPathsRevisiting :: String -> String -> String -> G -> Int
countPathsRevisiting rep s e g = dfs' g S.empty True s
  where
    dfs' g visited revisit x
      | x == e = 1
      | x == rep && revisit = sum $ map (dfs' g visited False) (expand x)
      | not (isBigCave x) && x `S.member` visited = 0
      | otherwise = sum $ map (dfs' g (x `S.insert` visited) revisit) (expand x)
    expand x = M.lookupDefault [] x g

smallCaves :: G -> [String]
smallCaves = filter (not . isBigCave) . M.keys

countPathsRevisitingOneSmallCave :: G -> Int
countPathsRevisitingOneSmallCave g =
  let
    caves = filter (\x -> x /= "start" && x /= "end") $ smallCaves g
    withoutRepeat = countPaths "start" "end" g
  in
    withoutRepeat + sum (map (\x -> countPathsRevisiting x "start" "end" g - withoutRepeat) caves)

part2 = countPathsRevisitingOneSmallCave . parseGraph

main :: IO ()
main = do
  input <- readFile "inputs/d12.txt"
  print $ part1 input
  print $ part2 input
