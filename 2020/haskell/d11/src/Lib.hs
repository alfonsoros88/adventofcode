module Lib
  ( Cell
  , toCell
  , converge
  , countOccupiedTotal
  , cellStep
  , computeCellNeighbours
  , cellStep2
  ) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)

data Cell
  = Empty
  | Occupied
  | Floor
  deriving (Eq, Show)

toCell :: Char -> Cell
toCell c =
  case c of
    'L' -> Empty
    '#' -> Occupied
    '.' -> Floor
    _ -> Floor

get :: [[Cell]] -> (Int, Int) -> Cell
get board (x, y) =
  let n = length board
      m = length $ head board
      row =
        if x >= 0 && x < n
          then board !! x
          else replicate m Floor
   in if y >= 0 && y < m
        then row !! y
        else Floor

neighbours :: [[Cell]] -> (Int, Int) -> [Cell]
neighbours board (x, y) =
  let coords =
        [ (i, j)
        | i <- fmap (+ x) [-1, 0, 1]
        , j <- fmap (+ y) [-1, 0, 1]
        , i /= x || j /= y
        ]
   in fmap (get board) coords

countOccupied :: [Cell] -> Int
countOccupied =
  foldl
    (\acc x ->
       if x == Occupied
         then acc + 1
         else acc)
    0

cellStep :: [[Cell]] -> (Int, Int) -> Cell
cellStep board coord =
  let neigh = neighbours board coord
      occ = countOccupied neigh
   in case get board coord of
        Empty ->
          if occ == 0
            then Occupied
            else Empty
        Occupied ->
          if occ >= 4
            then Empty
            else Occupied
        c -> c

step :: [[Cell]] -> ([[Cell]] -> (Int, Int) -> Cell) -> [[Cell]]
step board cellStep =
  let n = length board
      m = length $ head board
      getRow i = fmap (cellStep board) [(i, j) | j <- [0 .. (m - 1)]]
   in fmap getRow [0 .. (n - 1)]

converge :: [[Cell]] -> ([[Cell]] -> (Int, Int) -> Cell) -> [[Cell]]
converge board cellStep =
  let newBoard = step board cellStep
   in if newBoard == board
        then board
        else converge newBoard cellStep

countOccupiedTotal :: [[Cell]] -> Int
countOccupiedTotal = foldl (\acc x -> countOccupied x + acc) 0

size :: [[Cell]] -> (Int, Int)
size board = (length board, length $ head board)

findNextNeighbour ::
     [[Cell]] -> ((Int, Int) -> (Int, Int)) -> (Int, Int) -> Maybe (Int, Int)
findNextNeighbour board nextCoord coord@(x, y) =
  let cell = get board coord
      (n, m) = size board
      isIn (x, y) = x >= 0 && y >= 0 && x < n && y < m
      next = nextCoord coord
   in case cell of
        Floor ->
          if isIn next
            then findNextNeighbour board nextCoord next
            else Nothing
        _ -> Just coord

computeCellNeighbours :: [[Cell]] -> M.Map (Int, Int) [(Int, Int)]
computeCellNeighbours board =
  let (n, m) = size board
      coords = [(i, j) | i <- [0 .. (n - 1)], j <- [0 .. (m - 1)]]
      dirs =
        [ (\(x, y) -> (x + i, y + j))
        | i <- [-1, 0, 1]
        , j <- [-1, 0, 1]
        , i /= 0 || j /= 0
        ]
      neigh = fmap (\dir x -> findNextNeighbour board dir (dir x)) dirs
   in M.fromList [(c, mapMaybe ($ c) neigh) | c <- coords]

cellStep2 :: M.Map (Int, Int) [(Int, Int)] -> [[Cell]] -> (Int, Int) -> Cell
cellStep2 neighboursMap board coord =
  let neighCoords = fromMaybe [] $ M.lookup coord neighboursMap
      neighbours = fmap (get board) neighCoords
      occ = countOccupied neighbours
   in case get board coord of
        Empty ->
          if occ == 0
            then Occupied
            else Empty
        Occupied ->
          if occ >= 5
            then Empty
            else Occupied
        c -> c
