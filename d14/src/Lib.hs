module Lib
  ( parseInstruction
  , Mask
  , State(mem)
  , Mem
  , emptyState
  , step
  , step2
  ) where

import Data.List (isPrefixOf)

parseInstruction :: String -> Either Mask Mem
parseInstruction input
  | "mask" `isPrefixOf` input = Left (Mask (reverse $ last wds))
  | "mem" `isPrefixOf` input = Right (Mem place value)
  where
    wds = words input
    place = read $ init $ drop 4 $ head wds :: Int
    value = read $ last wds :: Int

newtype Mask =
  Mask String
  deriving (Show)

emptyMask = Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

maskLength = 36

data State =
  State
    { mask :: Mask
    , mem :: [(Int, Int)]
    }
  deriving (Show)

emptyState = State {mask = emptyMask, mem = []}

data Mem =
  Mem Int Int
  deriving (Show)

step :: State -> Either Mask Mem -> State
step s i =
  case i of
    Left m -> s {mask = m}
    Right (Mem p v) -> s {mem = res}
      where res = (p, applyMask (mask s) v) : mem s

step2 :: State -> Either Mask Mem -> State
step2 s i =
  case i of
    Left m -> s {mask = m}
    Right (Mem p v) -> s {mem = res}
      where res = mem s ++ [(a, v) | a <- maskAddress (mask s) p]

toBinary :: Int -> [Char]
toBinary 0 = ['0']
toBinary 1 = ['1']
toBinary n = bit : toBinary (n `div` 2)
  where
    bit =
      if even n
        then '0'
        else '1'

toInt :: [Char] -> Int
toInt bits = sum $ toPow bits 0
  where
    toPow :: [Char] -> Int -> [Int]
    toPow [] _ = []
    toPow (x:xs) n
      | x == '1' = 2 ^ n : toPow xs (n + 1)
      | otherwise = toPow xs (n + 1)

padWithZeros :: Int -> [Char] -> [Char]
padWithZeros n l = l ++ replicate r '0'
  where
    r = n - length l

applyMask :: Mask -> Int -> Int
applyMask (Mask m) x = toInt $ apply $ zip binary m
  where
    binary = padWithZeros maskLength (toBinary x)
    apply [] = []
    apply ((x, m):xs) =
      (if m == 'X'
         then x
         else m) :
      apply xs

maskAddress :: Mask -> Int -> [Int]
maskAddress (Mask m) a = toInt <$> binaries
  where
    binary = padWithZeros maskLength (toBinary a)
    binaries = applyMaskToAddress (zip binary m)

applyMaskToAddress :: [(Char, Char)] -> [[Char]]
applyMaskToAddress [] = [[]]
applyMaskToAddress ((x, m):xs) =
  case m of
    '0' -> (x :) <$> rest
    '1' -> ('1' :) <$> rest
    'X' -> (('0' :) <$> rest) ++ (('1' :) <$> rest)
  where
    rest = applyMaskToAddress xs
