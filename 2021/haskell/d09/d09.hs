module Main where

import Data.Char (digitToInt)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down(Down))
import Data.Vector (Vector)
import qualified Data.Vector as V

type CaveMap = Vector (Vector Int)

parseCave :: String -> CaveMap
parseCave = V.fromList . map (V.fromList . map digitToInt) . lines

dims :: CaveMap -> (Int, Int)
dims cave = (V.length cave, V.length $ cave V.! 0)

lk :: CaveMap -> (Int, Int) -> Maybe Int
lk cave (x, y) = (cave V.!? x) >>= (V.!? y)

adj :: (Int, Int) -> [(Int, Int)]
adj (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

isLocalMinimum :: CaveMap -> (Int, Int) -> Bool
isLocalMinimum cave (x, y) =
  let adjValues = mapMaybe (lk cave) . adj
   in case lk cave (x, y) of
        Nothing -> False
        Just c -> all (c <) (adjValues (x, y))

findLocalMinima :: CaveMap -> [(Int, Int)]
findLocalMinima cave = filter (isLocalMinimum cave) coords
  where
    (h, w) = dims cave
    coords = [(i, j) | i <- [0 .. h - 1], j <- [0 .. w - 1]]

part1 input = sum . map (+ 1) . mapMaybe (lk cave) $ findLocalMinima cave
  where
    cave = parseCave input

-- part2
growBasis :: CaveMap -> HashSet (Int, Int) -> [(Int, Int)] -> HashSet (Int, Int)
growBasis cave basis [] = basis
growBasis cave basis (x:xs) =
  let basis' = S.insert x basis
      toExpand =
        filter (not . (`S.member` basis')) $
        filter (maybe False (< 9) . lk cave) (adj x)
   in growBasis cave basis' (toExpand ++ xs)

connectedComponents :: CaveMap -> [HashSet (Int, Int)]
connectedComponents cave =
  let localMinima = findLocalMinima cave
      growEach :: [HashSet (Int, Int)] -> [(Int, Int)] -> [HashSet (Int, Int)]
      growEach basis [] = basis
      growEach basis (x:xs) =
        if not (any (S.member x) basis)
          then growEach (growBasis cave S.empty [x] : basis) xs
          else growEach basis xs
   in growEach [] localMinima

part2 input =
  product . take 3 . sortBy decreasingSize . map S.size . connectedComponents $
  cave
  where
    cave = parseCave input
    decreasingSize a b = compare (Down a) (Down b)

main :: IO ()
main = do
  input <- readFile "inputs/d09.txt"
  print $ part1 input
  print $ part2 input
