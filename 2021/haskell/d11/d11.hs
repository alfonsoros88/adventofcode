module Main where

import Data.Char (digitToInt, intToDigit)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.Maybe (mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V

newtype Grid =
  Grid
    { grid :: Vector (Vector Int)
    }

instance Show Grid where
  show = unlines . V.toList . V.map (V.toList . V.map intToDigit) . grid

readGrid :: String -> Grid
readGrid = Grid . V.fromList . map (V.fromList . map digitToInt) . lines

increaseEnergy :: Grid -> Grid
increaseEnergy = Grid . V.map (V.map (+ 1)) . grid

adj :: (Int, Int) -> [(Int, Int)]
adj (x, y) =
  [(i, j) | i <- [x - 1, x, x + 1], j <- [y - 1, y, y + 1], (i, j) /= (x, y)]

get :: Grid -> (Int, Int) -> Maybe Int
get (Grid g) (x, y) = g V.!? x >>= (V.!? y)

update :: Grid -> (Int, Int) -> (Int -> Int) -> Grid
update (Grid g) (x, y) f =
  case f <$> get (Grid g) (x, y) of
    Nothing -> Grid g
    Just v -> Grid $ g V.// [(x, g V.! x V.// [(y, v)])]

flash :: Grid -> (Int, Int) -> Grid
flash g k = foldl (\acc x -> update acc x (+ 1)) g (adj k)

toList :: Grid -> [(Int, (Int, Int))]
toList =
  concatMap (\(i, xs) -> [(v, (i, j)) | (j, v) <- xs]) .
  (zip [0 ..] . V.toList) . V.map (zip [0 ..] . V.toList) . grid

charged :: Grid -> [(Int, Int)]
charged = map snd . filter ((> 9) . fst) . toList

flashAll :: HashSet (Int, Int) -> Grid -> Grid
flashAll flashed g =
  let chargedList = charged g
      toFlash = S.difference (S.fromList chargedList) flashed
   in if S.null toFlash
        then g
        else let g' = foldl flash g (S.toList toFlash)
              in flashAll (S.union flashed toFlash) g'

countFlashed :: Grid -> Int
countFlashed = V.foldl (V.foldl (\acc x -> if x > 9 then acc + 1 else acc)) 0 . grid

resetCharged :: Grid -> Grid
resetCharged = Grid . V.map (V.map (\v -> if v > 9 then 0 else v)) . grid

step :: Grid -> (Int, Grid)
step g = let
    g' = flashAll S.empty . increaseEnergy $ g
    in (countFlashed g', resetCharged g')

nthStep :: Int -> Grid -> (Int, Grid)
nthStep n g = foldl (\(acc, g) _ -> let (x, g') = step g in (acc + x, g')) (0, g) [1 .. n]

part1 = fst . nthStep 100 . readGrid

-- part 2

dims :: Grid -> (Int, Int)
dims (Grid g) = (V.length g, V.length (g V.! 0))

findStepWithFlashEq :: Grid -> Int -> Int -> Int
findStepWithFlashEq g amount n =
  let (fs, g') = step g
   in if fs == amount
        then n
        else findStepWithFlashEq g' amount (n + 1)


part2 input = findStepWithFlashEq (readGrid input) (h * w) 1
    where
        g = readGrid input
        (h, w) = dims g

main :: IO ()
main = do
  input <- readFile "inputs/d11.txt"
  print $ part1 input
  print $ part2 input
