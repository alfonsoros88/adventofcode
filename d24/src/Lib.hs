module Lib
  ( part1
  , part2
  ) where

import Control.Monad (guard)
import Data.Biapplicative (second)
import Data.Bifunctor (bimap, first)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (group, isPrefixOf, nub, sort)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)

parsePath :: String -> [HexDirection]
parsePath [] = []
parsePath str
  | "e" `isPrefixOf` str = E : parsePath (tail str)
  | "se" `isPrefixOf` str = SE : parsePath (drop 2 str)
  | "sw" `isPrefixOf` str = SW : parsePath (drop 2 str)
  | "w" `isPrefixOf` str = W : parsePath (tail str)
  | "nw" `isPrefixOf` str = NW : parsePath (drop 2 str)
  | "ne" `isPrefixOf` str = NE : parsePath (drop 2 str)
  | otherwise = error "invalid direction"

-- Hexagonal direction
data HexDirection
  = E
  | SE
  | SW
  | W
  | NW
  | NE
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- Given a coordinate and a direction, return the new coordinate
move :: (Int, Int) -> HexDirection -> (Int, Int)
move (x, y) E = (x + 1, y)
move (x, y) SE = (x, y + 1)
move (x, y) SW = (x - 1, y + 1)
move (x, y) W = (x - 1, y)
move (x, y) NW = (x, y - 1)
move (x, y) NE = (x + 1, y - 1)

-- converts a hexagonal path to a coordinate
toCoord :: [HexDirection] -> (Int, Int)
toCoord = foldl move (0, 0)

-- Given a list of elements, count the number of occurrences of each element in the list
-- e.g. countOccurrences [1,2,3,1,2,3] == [(1,2),(3,1)]
countOccurrences :: (Eq a, Ord a) => [a] -> [(a, Int)]
countOccurrences = map (\xs -> (head xs, length xs)) . group . sort

-- Count the amount of odd nubmers in a list
countOdd :: [Int] -> Int
countOdd = length . filter odd

-- Parse the input into a list of coordinates
parseCoords :: String -> [(Int, Int)]
parseCoords = fmap (toCoord . parsePath) . lines

part1 :: String -> Int
part1 input = countOdd $ fmap snd occ
  where
    occ = countOccurrences $ parseCoords input

-- part2 
getBlackCoords :: [(Int, Int)] -> [(Int, Int)]
getBlackCoords =
  mapMaybe
    (\(c, a) ->
       if odd a
         then Just c
         else Nothing) .
  countOccurrences

type Blacks = HashSet (Int, Int)

neigh :: (Int, Int) -> [(Int, Int)]
neigh (x, y) =
  [ second succ
  , bimap pred succ
  , first pred
  , second pred
  , bimap succ pred
  , first succ
  ] <*>
  [(x, y)]

adjBlacks :: Blacks -> (Int, Int) -> Int
adjBlacks blacks coord = length . filter isBlack $ neigh coord
  where
    isBlack = flip HashSet.member blacks

nextGeneration :: Blacks -> Blacks
nextGeneration blacks = HashSet.fromList (survivors ++ newBlacks)
  where
    isBlack = flip HashSet.member blacks
    isOneOrTwo x = x == 1 || x == 2
    blackList = HashSet.toList blacks
    survivors = filter (isOneOrTwo . adjBlacks blacks) blackList
    whites = filter (not . isBlack) $ nub . sort $ concat $ neigh <$> blackList
    newBlacks = filter ((== 2) . adjBlacks blacks) whites

-- Compute the nth generatio
computeNthGen :: Int -> Blacks -> Blacks
computeNthGen n blks = foldl (\b f -> f b) blks (replicate n nextGeneration)

part2 input = length $ computeNthGen 100 blacks
  where
    blk = getBlackCoords $ parseCoords input
    blacks = HashSet.fromList blk
