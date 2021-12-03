module Main where

import Data.List (transpose)

data Bit
  = Zero
  | One
  deriving (Eq, Show)

-- Read bits from a string
readBits :: String -> [Bit]
readBits = map readBit
  where
    readBit '0' = Zero
    readBit '1' = One
    readBit _ = error "Invalid bit"

-- Return a list of bits from each line of the input file
readBitsFromFile :: String -> [[Bit]]
readBitsFromFile = map readBits . lines

-- Turn a Bits into a integer with base 10
bitsToInt :: [Bit] -> Int
bitsToInt = foldl (\acc bit -> acc * 2 + bitToInt bit) 0
  where
    bitToInt Zero = 0
    bitToInt One = 1

-- Count the number of Ones and Zeros in a bit list
countBits :: [Bit] -> (Int, Int)
countBits =
  foldl (\(zero, one) bit -> (zero + zeroBit bit, one + oneBit bit)) (0, 0)
  where
    zeroBit Zero = 1
    zeroBit One = 0
    oneBit Zero = 0
    oneBit One = 1

-- Return the most common bit from a list of Bits
mostCommonBit :: [Bit] -> Bit
mostCommonBit bits =
  if zeros > ones
    then Zero
    else One
  where
    (zeros, ones) = countBits bits

-- Return the most common bit from each column of the input bit matrix
mostCommonBitColumnWise = map mostCommonBit . transpose

-- Calculate the gamma rate from a input bit matrix
gammaRate :: [[Bit]] -> Int
gammaRate = bitsToInt . mostCommonBitColumnWise

flipBit :: Bit -> Bit
flipBit Zero = One
flipBit One = Zero

-- Negate a list of bits
flipBits :: (Functor f) => f Bit -> f Bit
flipBits = fmap flipBit

-- Calculate the epsilon rate from a input bit matrix
epsilonRate :: [[Bit]] -> Int
epsilonRate = bitsToInt . flipBits . mostCommonBitColumnWise

-- Compute the power consumption out of the multiplication of the gamma and
-- epsion rates
powerConsumption :: [[Bit]] -> Int
powerConsumption = (*) <$> gammaRate <*> epsilonRate

-- Comppute the power consumption of the input bit matrix
part1 = powerConsumption . readBitsFromFile

-- part2
-- Get the nth column of the imput bit matrix
nthColumn :: Int -> [[Bit]] -> [Bit]
nthColumn n = map (!! n)

countNthColumnBits :: Int -> [[Bit]] -> (Int, Int)
countNthColumnBits n = countBits . nthColumn n

mostCommonNthColumnBit :: Int -> [[Bit]] -> Bit
mostCommonNthColumnBit n = mostCommonBit . nthColumn n

filterNthColumnEq :: Int -> Bit -> [[Bit]] -> [[Bit]]
filterNthColumnEq n bit = filter (\row -> (row !! n) == bit)

filterBitCriteria :: (Int -> [[Bit]] -> Bit) -> Int -> [[Bit]] -> [[Bit]]
filterBitCriteria f n bits = filterNthColumnEq n (f n bits) bits

oxigenGeneratorFilterCriteria :: Int -> [[Bit]] -> [[Bit]]
oxigenGeneratorFilterCriteria = filterBitCriteria mostCommonNthColumnBit

rowSize :: [[Bit]] -> Int
rowSize = length . head

filterTillLast :: (Int -> [[Bit]] -> [[Bit]]) -> Int -> Int -> [[Bit]] -> [Bit]
filterTillLast fc _ _ [x] = x
filterTillLast fc n m ls = filterTillLast fc (n + 1 `mod` m) m $ fc n ls

oxygenGeneratorRate :: [[Bit]] -> Int
oxygenGeneratorRate bits =
  bitsToInt $ filterTillLast oxigenGeneratorFilterCriteria 0 (rowSize bits) bits

co2ScrubberFilterCruteria :: Int -> [[Bit]] -> [[Bit]]
co2ScrubberFilterCruteria =
  filterBitCriteria (\x y -> flipBit $ mostCommonNthColumnBit x y)

co2ScrubberRate :: [[Bit]] -> Int
co2ScrubberRate bits =
  bitsToInt $ filterTillLast co2ScrubberFilterCruteria 0 (rowSize bits) bits

-- Return the life support rate as the multiplication of the oxigen generator
-- rate times the co2 scrubber rate
lifeSupportRate :: [[Bit]] -> Int
lifeSupportRate = (*) <$> oxygenGeneratorRate <*> co2ScrubberRate

part2 = lifeSupportRate . readBitsFromFile

main :: IO ()
main = do
  input <- readFile "inputs/d03.txt"
  print $ part1 input
  print $ part2 input
