module Lib
  ( someFunc
  , Policy(..)
  , Range(..)
  , complies
  , compliesUpdatedPolicy
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Range =
  Range
    { low :: Int
    , high :: Int
    }
  deriving (Show)

inRange :: Range -> Int -> Bool
inRange r n = (low r) <= n && (high r) >= n

data Policy =
  Policy
    { range :: Range
    , letter :: Char
    }
  deriving (Show)

letterInstances :: Char -> String -> Int
letterInstances c s = foldl (compare) 0 s
  where
    compare b l
      | l == c = b + 1
      | otherwise = b

complies :: Policy -> String -> Bool
complies p s = inRangeP $ countLetter s
  where
    countLetter = letterInstances $ letter p
    inRangeP = inRange $ range p

-- part 2

compliesUpdatedPolicy :: Policy -> String -> Bool
compliesUpdatedPolicy p s = (firstEqual || secondEqual) && not (firstEqual && secondEqual)
  where
    r = range p
    l = low r
    h = high r
    c = letter p
    firstLetter = s !! (l - 1)
    firstEqual = firstLetter == c
    secondLetter = s !! (h - 1)
    secondEqual = secondLetter == c
