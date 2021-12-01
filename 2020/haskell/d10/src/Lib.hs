module Lib
  ( count1sAnd3sDiffs
  , countAlternatives
  , countAlternativesDP
  ) where

diffs :: [Int] -> [Int]
diffs ls = zipWith (-) (tail ls) (init ls)

count1sAnd3s :: (Int, Int) -> Int -> (Int, Int)
count1sAnd3s (ones, threes) x
  | x == 1 = (ones + 1, threes)
  | x == 3 = (ones, threes + 1)

count1sAnd3sDiffs :: [Int] -> (Int, Int)
count1sAnd3sDiffs numbers =
  let start = 0
      end = last numbers + 3
      adapters = 0 : numbers ++ [end]
      ds = diffs adapters
   in foldl count1sAnd3s (0, 0) ds

countAlternatives :: [Int] -> Int
countAlternatives numbers =
  let start = 0
      end = last numbers + 3
      countRec :: (Int, Int) -> [Int] -> Int
      countRec (s, e) [] =
        if e - s <= 3
          then 1
          else 0
      countRec (s, e) (x:xs) =
        if x - s <= 3
          then countRec (s, e) xs + countRec (x, e) xs
          else 0
   in countRec (start, end) numbers

countAlternativesDP :: [Int] -> Int
countAlternativesDP numbers =
  let end = 3 + last numbers
      ls = reverse numbers
      firstRow =
        [ if end - s <= 3
          then 1
          else 0
        | s <- [0 .. end]
        ]
      nextRow :: Int -> [Int] -> Int -> [Int] -> [Int]
      nextRow (-1) _ _ xs = xs
      nextRow l lastRow e xs =
        nextRow
          (l - 1)
          lastRow
          e
          ((if e - l <= 3
              then lastRow !! l + lastRow !! e
              else 0) :
           xs)
      finalRow = foldl (\row x -> nextRow (end - 1) row x []) firstRow ls
   in head finalRow
