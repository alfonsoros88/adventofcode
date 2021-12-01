module Lib
  ( toField
  , notMatchingNumber
  , matchingFields
  , findSolution
  , sortFields
  , Field(name)
  ) where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (find)
import Data.List.Split
import Data.Maybe (fromJust)
import Data.Ord (compare)
import Data.Sort (sort, sortBy)

newtype Range =
  Range (Int, Int)
  deriving (Show)

data Field =
  Field
    { name :: String
    , r1 :: Range
    , r2 :: Range
    }
  deriving (Show)

toField :: String -> Field
toField str = Field {name = n, r1 = Range (a, b), r2 = Range (c, d)}
  where
    n:ranges:_ = splitOn ":" str
    ws = words ranges
    a:b:_ = read <$> splitOn "-" (head ws)
    c:d:_ = read <$> splitOn "-" (ws !! 2)

inRange :: Range -> Int -> Bool
inRange (Range (a, b)) n = a <= n && n <= b

matchWithField :: Int -> Field -> Bool
matchWithField n f = inFirstRange || inSecondRange
  where
    inFirstRange = inRange (r1 f) n
    inSecondRange = inRange (r2 f) n

notMatchingNumber :: [Field] -> [Int] -> Maybe Int
notMatchingNumber _ [] = Nothing
notMatchingNumber fs (x:xs) =
  if any (matchWithField x) fs
    then notMatchingNumber fs xs
    else Just x

matchingFields :: [Int] -> [Field] -> [Int]
matchingFields numbers fields = fst <$> matching
  where
    enumerate = zip [0 ..]
    predicate f = all (`matchWithField` f) numbers
    matching = filter (predicate . snd) $ enumerate fields

findSolution :: [[Int]] -> [Int]
findSolution ls = solution
  where
    enumerate = zip [0 ..]
    lengthCmp a b = compare (length $ snd a) (length $ snd b)
    sorted = sortBy lengthCmp $ enumerate ls
        --
    findNext _ [] = []
    findNext s ((i, x):xs) = (i, y) : findNext s' xs
      where
        y = fromJust $ find (\x -> not $ IntSet.member x s) x
        s' = IntSet.insert y s
        --
    solution = snd <$> sort (findNext IntSet.empty sorted)

sortFields :: [Int] -> [Field] -> [Field]
sortFields [] _ = []
sortFields (x:xs) fields = fields !! x : sortFields xs fields
