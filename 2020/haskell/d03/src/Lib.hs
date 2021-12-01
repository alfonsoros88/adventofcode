module Lib
  ( ForestTile(..)
  , countTreesFromRoot
  ) where

import Relude.List

newtype ForestTile =
  ForestTile
    { matrix :: [String]
    }
  deriving (Show)

data ForestCell
  = Tree
  | Empty
  deriving (Show)

toForestCell :: Char -> Maybe ForestCell
toForestCell c =
  case c of
    '#' -> Just Tree
    '.' -> Just Empty
    otherwise -> Nothing

getCell :: ForestTile -> (Int, Int) -> Maybe ForestCell
getCell f (x, y) = row f >>= col >>= toForestCell
  where
    row = getRow x
    col = getCol y

getRow :: Int -> ForestTile -> Maybe String
getRow n f = (matrix f) !!? n

getCol :: Int -> String -> Maybe Char
getCol n s = s !!? c
  where
    l = length s
    c = n `mod` l

sumCoord :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumCoord (x, y) (u, v) = (x + u, y + v)

countTrees :: ForestTile -> (Int, Int) -> (Int, Int) -> Int -> Int
countTrees f inc coord count = case cell of
    Just Tree -> countTrees f inc nextCoord (count + 1)
    Just Empty -> countTrees f inc nextCoord count
    Nothing -> count
    where
        cell = getCell f coord
        nextCoord = sumCoord coord inc

countTreesFromRoot :: ForestTile -> (Int, Int) -> Int
countTreesFromRoot f inc = countTrees f inc (0, 0) 0

