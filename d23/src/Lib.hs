module Lib
  ( Cups(Cups)
  , toNumList
  , move
  , moven
  , toBigCups
  , moveBig
  , part1Score
  , part2score
  ) where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Debug.Trace

import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Takes a string with only numbers and returns a list of numbers in the string in order
-- Example: "12345" -> [1,2,3,4,5]
toNumList = map (\x -> read [x] :: Int)

newtype Cups =
  Cups [Int]

-- Converts the input string to a Cups
-- Example: "12345" -> Cups [1,2,3,4,5]
instance Read Cups where
  readsPrec _ input = [(Cups $ toNumList input, "")]

-- Shows a Cups as a string
-- Example: Cups [1,2,3,4,5] -> "12345"
instance Show Cups where
  show (Cups xs) = show xs

part1Score :: Cups -> String
part1Score (Cups xs) = concatMap show $ tail after ++ before
  where
    pos = fromMaybe 0 $ elemIndex 1 xs
    (before, after) = splitAt pos xs

-- Takes a number from 1 to 10 and decrements it by 1 and returns the new
-- number. If the number is 1, it returns 10.
-- Example: 1 -> 9, 2 -> 1, 3 -> 2, etc.
decrement :: Int -> Int
decrement n =
  if n == 1
    then 9
    else n - 1

move :: Cups -> Cups
move (Cups []) = Cups []
move (Cups (x:xs)) =
  let (picked, rest) = splitAt 3 xs
      findTarget t =
        if t `elem` picked
          then findTarget (decrement t)
          else t
      target = findTarget (decrement x)
      available = rest ++ [x]
      pos = fromMaybe 0 $ elemIndex target available
      (before, after) = splitAt pos available
   in Cups $ before ++ [head after] ++ picked ++ tail after

-- Move the cups n times and return the final state of the cups.
moven :: Int -> Cups -> Cups
moven n cups = foldl (\c _ -> move c) cups [1 .. n]

newtype BigCups =
  BigCups (UArray Int Int)

bigcupsMaxLabel = 1000000

bigcupsMaxIndex = bigcupsMaxLabel - 1

-- Shows a BigCups as a string of numbers
instance Show BigCups where
  show (BigCups a) = show $ elems a

toBigCups :: Cups -> BigCups
toBigCups (Cups xs) =
  BigCups $
  runSTUArray $ do
    arr <- newListArray (0, bigcupsMaxIndex) [1 ..]
    copyList arr 0 xs
    return arr

-- Takes a mutable array, an index and a value list and sets the value at the
-- index to the value list.
copyList :: (MArray a e m) => a Int e -> Int -> [e] -> m ()
copyList _ _ [] = return ()
copyList a i (x:xs) = do
  writeArray a i x
  copyList a (i + 1) xs

moveBig :: BigCups -> BigCups
moveBig (BigCups xs) =
  let picked = fmap ((!) xs) [1, 2, 3]
      dec n =
        if n == 1
          then bigcupsMaxLabel
          else n - 1
      findTarget t =
        if t `elem` picked
          then findTarget (dec t)
          else t
      target = findTarget (dec (xs ! 0))
      els = elems xs
      available = drop 4 els
      (before, after) =
        case elemIndex target available of
          Just x -> splitAt (x + 1) available
          Nothing -> (available, [])
   in BigCups $
      runSTUArray $ do
        dst <- newArray (0, bigcupsMaxIndex) 0
        copyList dst 0 before
        copyList dst (length before) picked
        copyList dst ((length before) + 3) after
        copyList dst ((length before) + 3 + (length after)) [head els]
        return dst

part2score :: BigCups -> String
part2score (BigCups xs) = show $ product [i | i <- take 2 (after ++ before)]
  where
    els = elems xs
    pos = fromMaybe 0 $ elemIndex 1 els
    (before, after) = splitAt (pos + 1) els
