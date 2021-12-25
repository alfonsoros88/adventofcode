module Main where
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Data.Bifunctor (first, bimap)
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import qualified Data.Vector.Unboxed.Mutable as S

data Area = Area { xrange :: (Int, Int), yrange :: (Int, Int) } deriving (Show)

tuple2 :: [a] -> (a, a)
tuple2 [x, y] = (x, y)
tuple2 _ = error "tuple2: invalid input"

readTargetRanges :: String -> ((Int, Int), (Int, Int))
readTargetRanges = bimap (f . init) f . tuple2 . map (drop 2) . drop 2 . words
    where f = tuple2 . map read . splitOn ".."

toArea :: ((Int, Int), (Int, Int)) -> Area
toArea (w, h) = Area w h

targetList :: Area -> [(Int, Int)]
targetList a = [(x, y) | x <- range $ xrange a, y <- range $ yrange a]
    where range (a, b) = [a, a + signum (b - a)..b]

-- distance traversed in `t` steps with initial velocity `x`: d = (2 * x - t) * (t + 1) / 2
--
-- We want to hit the input distance `d` exactly, therefore `x` must be in the range
-- [1 .. d] and `t` must be in the range [1 .. x - 1]
--
-- if `t` is equal to `x` then the prove stops exactly at distance `d` at
-- time `x`, meaning that any other time `t' > t` is valid for that particular
-- `x`.
--
speedTimesX :: Int -> [(Int, Int)]
speedTimesX d = [(x, t) | x <- [0..d], t <- [0..x - 1], ((2 * x - t) * (t + 1)) `quotRem` 2 == (d, 0)]

speedTimesY :: Int -> [(Int, Int)]
speedTimesY d = expandY [(y, t) | y <- [0..d], t <- [0..d], ((2 * y + t) * (t + 1)) `quotRem` 2 == (d, 0)]

expandY :: [(Int, Int)] -> [(Int, Int)]
expandY = concatMap (\(y, t) -> [(-y, t), (y - 1, t + 2 * y - 1)])

speeds :: (Int, Int) -> [(Int, Int)]
speeds (x, y) = let
    xs = speedTimesX x
    ys = speedTimesY (abs y)
    go :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
    go _ [] = []
    go (x, t) ((y, t'):ys)
        | t == t' = (x, y) : go (x, t) ys
        | t == x - 1 && t' >= t = (x, y) : go (x, t) ys
        | otherwise = go (x, t) ys
    in concat $ ($ ys) <$> (go <$> xs)

joinSpeeds :: [[(Int, Int)]] -> HashSet (Int, Int)
joinSpeeds = foldl S.union S.empty . map S.fromList

trajectory :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
trajectory (x, y) (vx, vy) = (x', y') : trajectory (x', y') (vx', vy')
    where
        x' = x + vx
        y' = y + vy
        vx' = vx - signum vx
        vy' = vy - 1

insideArea :: Area -> (Int, Int) -> Bool
insideArea (Area (x1, x2) (y1, y2)) (x, y) = x >= x1 && x <= x2 && y >= y1 && y <= y2

verify :: Area -> (Int, Int) -> Bool
verify a (x, y) = any (insideArea a) points
    where
        points = takeWhile (\(x', y') -> x' <= snd (xrange a) && y' >= fst (yrange a)) $ trajectory (0, 0) (x, y)

-- if y is positive the max height reached is y * (y + 1) / 2
maxHeight :: Int -> Int
maxHeight y = if y > 0 then y * (y + 1) `div` 2 else 0

part1 = maxHeight . maximum . map snd . S.toList . joinSpeeds . map speeds . targetList . toArea . readTargetRanges

--------------------------------------------------------------------------------
-- Part 2

part2 input = S.size . joinSpeeds . map speeds . targetList $ a
    where
        a = toArea . readTargetRanges $ input

main :: IO ()
main = do
  input <- readFile "inputs/d17.txt"
  print $ part1 input
  print $ part2 input
