{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List.Split (splitOn)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Either (partitionEithers)

-- Split the input string by empty lines
splitByEmptyLines :: String -> [String]
splitByEmptyLines = splitOn "\n\n"

data Bingo =
  Bingo
    { numbers :: HashMap Int (Int, Int)
    , board :: Vector (Vector (Bool, Int))
    }
  deriving (Show)

parseBoard :: String -> [[Int]]
parseBoard = map (map read . words) . lines

indexElements :: [[Int]] -> [(Int, (Int, Int))]
indexElements board =
  concatMap (\(i, row) -> [(e, (i, j)) | (j, e) <- zip [0 ..] row]) enumRows
  where
    enumRows = zip [0 ..] board

elemMap :: [[Int]] -> HashMap Int (Int, Int)
elemMap board = HM.fromList $ indexElements board

toVector :: [[a]] -> Vector (Vector a)
toVector = V.fromList . map V.fromList

parseBingo :: String -> Bingo
parseBingo input = Bingo {numbers, board}
  where
    b = parseBoard input
    numbers = elemMap b
    board = toVector $ map (map ((,) False)) b

nthColumn :: Int -> Vector (Vector a) -> Vector a
nthColumn n = V.map (V.! n)

nthRow :: Int -> Vector (Vector a) -> Vector a
nthRow n = (V.! n)

full :: Vector (Bool, Int) -> Bool
full = V.all fst

mark :: Int -> Int -> Bingo -> Bingo
mark i j bingo = bingo {board = marked}
  where
    b = board bingo
    n = snd $ (b V.! i) V.! j
    ith = (b V.! i) V.// [(j, (True, n))]
    marked = b V.// [(i, ith)]

type Winner = Bingo

updateBingo :: Int -> Bingo -> Either Bingo Winner
updateBingo n bingo =
  case HM.lookup n (numbers bingo) of
    Nothing -> Left bingo
    Just (i, j) ->
      let bingo' = mark i j bingo
          b = board bingo'
          row = nthRow i b
          col = nthColumn j b
       in if full row || full col
            then Right bingo'
            else Left bingo'

unmarked :: Bingo -> [Int]
unmarked = V.toList . V.map snd . V.concatMap (V.filter (not . fst)) . board

playBingo :: [Bingo] -> [Int] -> (Int, Winner)
playBingo bs [] = error "No more numbers"
playBingo bs (x:xs) =
  if (length winners > 0)
    then (x, head winners)
    else playBingo  bs' xs
  where
    (bs', winners) = partitionEithers $ map (updateBingo x) bs

part1 input =
  let (x:xs) = splitByEmptyLines input
      numbers :: [Int]
      numbers = map read $ splitOn "," x
      boards = map (parseBingo) xs
      (n, winner) = playBingo boards numbers
    in n * (sum $ unmarked winner)

-- part 2

playBingoTillLast :: [Bingo] -> [Int] -> [(Int, Winner)]
playBingoTillLast [] _ = []
playBingoTillLast bs [] = error "No more numbers"
playBingoTillLast bs (x:xs) = (((,) x) <$> winners) ++ playBingoTillLast bs' xs
  where
    (bs', winners) = partitionEithers $ map (updateBingo x) bs

part2 input =
  let (x:xs) = splitByEmptyLines input
      numbers :: [Int]
      numbers = map read $ splitOn "," x
      boards = map (parseBingo) xs
      winners = playBingoTillLast boards numbers
      (n, lastWinner) = last winners
    in n * (sum $ unmarked lastWinner)

main :: IO ()
main = do
  input <- readFile "inputs/d04.txt"
  print $ part1 input
  print $ part2 input
