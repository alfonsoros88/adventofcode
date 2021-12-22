module Main where

import Data.Bifunctor (first)
import Control.Monad (guard, when)
import Data.Maybe (fromJust)

data Bit
  = Zero
  | One
  deriving (Eq)

instance Show Bit where
  show Zero = "0"
  show One = "1"

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One = 1

hexToBits :: Char -> [Bit]
hexToBits '0' = [Zero, Zero, Zero, Zero]
hexToBits '1' = [Zero, Zero, Zero, One]
hexToBits '2' = [Zero, Zero, One, Zero]
hexToBits '3' = [Zero, Zero, One, One]
hexToBits '4' = [Zero, One, Zero, Zero]
hexToBits '5' = [Zero, One, Zero, One]
hexToBits '6' = [Zero, One, One, Zero]
hexToBits '7' = [Zero, One, One, One]
hexToBits '8' = [One, Zero, Zero, Zero]
hexToBits '9' = [One, Zero, Zero, One]
hexToBits 'A' = [One, Zero, One, Zero]
hexToBits 'B' = [One, Zero, One, One]
hexToBits 'C' = [One, One, Zero, Zero]
hexToBits 'D' = [One, One, Zero, One]
hexToBits 'E' = [One, One, One, Zero]
hexToBits 'F' = [One, One, One, One]
hexToBits _ = []

readBits :: String -> [Bit]
readBits = concatMap hexToBits

bitsToInt :: [Bit] -> Int
bitsToInt = foldl (\n b -> n * 2 + bitToInt b) 0

data Packet
  = Literal
      { ver :: Int
      , val :: Int
      }
  | Op
      { ver :: Int
      , typ :: Int
      , packs :: [Packet]
      }
    deriving (Show)

splitAtMaybe :: Int -> [a] -> Maybe ([a], [a])
splitAtMaybe n xs = do
    guard (n > 0 && n <= length xs)
    return $ splitAt n xs

parseLiteral :: [Bit] -> Maybe (Int, [Bit])
parseLiteral bits =
  let go n bs = do
        (x:xs, rest) <- splitAtMaybe 5 bs
        if x == Zero
          then return (n * 16 + bitsToInt xs, rest)
          else go (n * 16 + bitsToInt xs) rest
   in go 0 bits

compParsers :: Monoid a => ([Bit] -> Maybe (a, [Bit])) -> ([Bit] -> Maybe (a, [Bit])) -> ([Bit] -> Maybe (a, [Bit]))
compParsers f g x = do
    (x, xs) <- f x
    (y, ys) <- g xs
    return (x `mappend` y, ys)

tryConsume :: ([Bit] -> Maybe (a, [Bit])) -> [Bit] -> [a]
tryConsume f xs =
    case f xs of
        Nothing -> mempty
        Just (x, xs) -> x : tryConsume f xs

parsePacket :: [Bit] -> Maybe (Packet, [Bit])
parsePacket bits = do
    (ver, rest) <- first bitsToInt <$> splitAtMaybe 3 bits
    (typ, rest) <- first bitsToInt <$> splitAtMaybe 3 rest
    case typ of
      4 -> do
        (val, rest) <- parseLiteral rest
        pure (Literal ver val, rest)
      _ -> do
        (opid, rest) <- first bitsToInt <$> splitAtMaybe 1 rest
        case opid of
            0 -> do
                (l, rest) <- first bitsToInt <$> splitAtMaybe 15 rest
                (xs, rest) <- splitAtMaybe l rest
                let packs = tryConsume parsePacket xs
                pure (Op ver typ packs, rest)
            1 -> do
                (l, rest) <- first bitsToInt <$> splitAtMaybe 11 rest
                (packs, rest) <- foldl1 compParsers (replicate l (fmap (first (: [])) . parsePacket)) rest
                pure (Op ver typ packs, rest)
            _ -> error "Unknown opid"

sumVersions :: Packet -> Int
sumVersions p = case p of
        Literal v _ -> v
        Op v _ ps -> v + sum (sumVersions <$> ps)

part1 = sumVersions . fst .fromJust . parsePacket . readBits

--------------------------------------------------------------------------------
-- Part 2

eval :: Packet -> Int
eval p = case p of
    Literal _ v -> v
    Op _ t ps -> case t of
        0 -> sum (eval <$> ps)
        1 -> product (eval <$> ps)
        2 -> minimum (eval <$> ps)
        3 -> maximum (eval <$> ps)
        5 -> if eval (head ps) > eval (ps !! 1) then 1 else 0
        6 -> if eval (head ps) < eval (ps !! 1) then 1 else 0
        7 -> if eval (head ps) == eval (ps !! 1) then 1 else 0
        _ -> error "Unknown opcode"

part2 = eval . fst .fromJust . parsePacket . readBits

main :: IO ()
main = do
  input <- readFile "inputs/d16.txt"
  print $ part1 input
  print $ part2 input
