module Main where

import Control.Monad (guard)
import Data.Char (digitToInt, intToDigit)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Heap (MinHeap)
import qualified Data.Heap as H
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace (trace)

data G =
  G
    { grid :: Vector (Vector Int)
    , neigh :: (Int, Int) -> [(Int, Int)]
    , cost :: (Int, Int) -> Int
    , dims :: (Int, Int)
    }

readMat :: String -> Vector (Vector Int)
readMat = V.map (V.map digitToInt . V.fromList) . V.fromList . lines

val :: Vector (Vector Int) -> (Int, Int) -> Int
val v (x, y) = (V.! y) $ v V.! x

boundGuard :: Int -> Int -> (Int, Int) -> Maybe (Int, Int)
boundGuard h w (x, y) = do
  guard $ x >= 0 && x < h
  guard $ y >= 0 && y < w
  return (x, y)

gridNeighbors :: Int -> Int -> (Int, Int) -> [(Int, Int)]
gridNeighbors h w (x, y) =
  mapMaybe (boundGuard h w) [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]

makeGPart1 :: Vector (Vector Int) -> G
makeGPart1 v =
  G {grid = v, neigh = gridNeighbors h w, cost = val v, dims = (h, w)}
    where
        h = V.length v
        w = V.length $ V.head v

dijkstra ::
     G
  -> MinHeap (Int, (Int, Int))
  -> HashMap (Int, Int) Int
  -> HashMap (Int, Int) Int
dijkstra g mh dist =
  case H.view mh of
    Nothing -> dist
    Just ((d, p), h) -> dijkstra g h' dist'
      where ns = neigh g p
            len = cost g
            update ::
                 (MinHeap (Int, (Int, Int)), HashMap (Int, Int) Int)
              -> (Int, Int)
              -> (MinHeap (Int, (Int, Int)), HashMap (Int, Int) Int)
            update (heap, dist) n =
              let alt = len n + d
                  cond = (> len n + d) <$> M.lookup n dist
               in if fromMaybe True cond
                    then (H.insert (alt, n) heap, M.insert n alt dist)
                    else (heap, dist)
            (h', dist') = foldl update (h, dist) ns

shortestPath :: G -> (Int, Int) -> (Int, Int) -> Int
shortestPath g start end =
  dijkstra g (H.singleton (0, start)) (M.singleton start 0) M.! end

part1 input =
  let g = makeGPart1 . readMat $ input
      (h, w) = dims g
   in shortestPath g (0, 0) (h - 1, w - 1)

--------------------------------------------------------------------------------
-- Part 2

cost2 :: Vector (Vector Int) -> (Int, Int) -> Int
cost2 v (x, y) = let
    h = V.length v
    w = V.length . V.head $ v
    i = x `div` h
    j = y `div` w
    x' = x `mod` h
    y' = y `mod` w
    a =  val v (x', y') + i + j
    in if a > 9 then 1 + (a `mod` 10) else a

makeGPart2 :: Vector (Vector Int) -> G
makeGPart2 v =
  G {grid = v, neigh = gridNeighbors h w, cost = cost2 v, dims = (h, w)}
    where
        h = 5 * (V.length v)
        w = 5 * (V.length $ V.head v)

part2 input =
  let g = makeGPart2 . readMat $ input
      (h, w) = dims g
   in shortestPath g (0, 0) (h - 1, w - 1)

main :: IO ()
main = do
  input <- readFile "inputs/d15.txt"
  print $ part1 input
  print $ part2 input
