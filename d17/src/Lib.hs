module Lib
  ( readInput
  , computeNextGen
  , computeNextGen4d
  , applyNTimes
  , countActive
  , countActive4d
  ) where

import Data.Maybe (fromMaybe)

grid :: (Int, Int, Int) -> [(Int, Int, Int)]
grid (x, y, z) =
  [ (x', y', z')
  | x' <- [x - 1, x, x + 1]
  , y' <- [y - 1, y, y + 1]
  , z' <- [z - 1, z, z + 1]
  ]

neighborCoords :: (Int, Int, Int) -> [(Int, Int, Int)]
neighborCoords (x, y, z) =
  [(x', y', z') | (x', y', z') <- grid (x, y, z), x /= x' || y /= y' || z /= z']

nCoords4d :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
nCoords4d (x, y, z, w) =
  [ (x', y', z', w')
  | (x', y', z') <- grid (x, y, z)
  , w' <- [w - 1, w, w + 1]
  , x /= x' || y /= y' || z /= z' || w /= w'
  ]

getnth :: Int -> [a] -> Maybe a
getnth _ [] = Nothing
getnth n (x:xs)
  | n < 0 = Nothing
  | n == 0 = Just x
  | otherwise = getnth (n - 1) xs

lk :: [[[Char]]] -> (Int, Int, Int) -> Char
lk m (a, b, c) = fromMaybe '.' (getnth a m >>= getnth b >>= getnth c)

lk4d :: [[[[Char]]]] -> (Int, Int, Int, Int) -> Char
lk4d m (a, b, c, d) =
  fromMaybe '.' (getnth a m >>= getnth b >>= getnth c >>= getnth d)

countN :: [[[Char]]] -> (Int, Int, Int) -> Int
countN m c =
  sum $
  fmap
    (\x ->
       if x == '.'
         then 0
         else 1)
    neigh
  where
    neigh = lk m <$> neighborCoords c

countN4d :: [[[[Char]]]] -> (Int, Int, Int, Int) -> Int
countN4d m c =
  sum $
  fmap
    (\x ->
       if x == '.'
         then 0
         else 1)
    neigh
  where
    neigh = lk4d m <$> nCoords4d c

dims :: [[[Char]]] -> (Int, Int, Int)
dims m = (x, y, z)
  where
    x = length m
    y = length $ head m
    z = length $ head $ head m

dims4d :: [[[[Char]]]] -> (Int, Int, Int, Int)
dims4d m = (x, y, z, w)
  where
    x = length m
    (y, z, w) = dims $ head m

toCoords :: (Int, Int, Int) -> [(Int, Int, Int)]
toCoords (x, y, z) =
  [(a, b, c) | a <- [0 .. (x - 1)], b <- [0 .. (y - 1)], c <- [0 .. (z - 1)]]

toCoords4d :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
toCoords4d (x, y, z, w) =
  [(a, b, c, d) | (a, b, c) <- toCoords (x, y, z), d <- [0 .. (w - 1)]]

nextGen :: [[[Char]]] -> (Int, Int, Int) -> Char
nextGen m c =
  case x of
    '.' ->
      if n == 3
        then '#'
        else '.'
    '#' ->
      if n >= 2 && n <= 3
        then '#'
        else '.'
  where
    x = lk m c
    n = countN m c

nextGen4d :: [[[[Char]]]] -> (Int, Int, Int, Int) -> Char
nextGen4d m c =
  case x of
    '.' ->
      if n == 3
        then '#'
        else '.'
    '#' ->
      if n >= 2 && n <= 3
        then '#'
        else '.'
  where
    x = lk4d m c
    n = countN4d m c

nonEmptyBorders1d :: [Char] -> Bool
nonEmptyBorders1d (x:xs) = x == '#' || l == '#'
  where
    l = last xs

nonEmptyBorders2d :: [[Char]] -> Bool
nonEmptyBorders2d (x:xs) = hasActive x || any nonEmptyBorders1d m || hasActive b
  where
    hasActive = elem '#'
    h = x
    m = init xs
    b = last xs

nonEmptyBorders3d :: [[[Char]]] -> Bool
nonEmptyBorders3d (x:xs) = hasActive x || any nonEmptyBorders2d m || hasActive b
  where
    hasActive = any (elem '#')
    m = init xs
    b = last xs

nonEmptyBorders4d :: [[[[Char]]]] -> Bool
nonEmptyBorders4d (x:xs) = hasActive x || any nonEmptyBorders3d m || hasActive b
  where
    hasActive = any (any (elem '#'))
    m = init xs
    b = last xs

readInput :: String -> [[[Char]]]
readInput input = [lines input]

expand1d :: [Char] -> [Char]
expand1d x = '.' : x ++ ['.']

expand2d :: [[Char]] -> [[Char]]
expand2d x = dots : x' ++ [dots]
  where
    x' = fmap expand1d x
    n = length $ head x'
    dots = replicate n '.'

expand3d :: [[[Char]]] -> [[[Char]]]
expand3d x = plane : x' ++ [plane]
  where
    x' = fmap expand2d x
    h = head x'
    n = length h
    m = length $ head h
    plane = replicate n (replicate m '.')

expand4d :: [[[[Char]]]] -> [[[[Char]]]]
expand4d x = cube : x' ++ [cube]
  where
    x' = fmap expand3d x
    h = head x'
    (n, m, w) = dims h
    cube = replicate n (replicate m (replicate w '.'))

prepare :: [[[Char]]] -> [[[Char]]]
prepare m =
  if nonEmptyBorders3d m
    then expand3d m
    else m

prepare4d :: [[[[Char]]]] -> [[[[Char]]]]
prepare4d m =
  if nonEmptyBorders4d m
    then expand4d m
    else m

update1d :: [Char] -> Int -> Char -> [Char]
update1d (x:xs) 0 c = c : xs
update1d (x:xs) n c = x : update1d xs (n - 1) c

update2d :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
update2d (x:xs) (0, n) c = update1d x n c : xs
update2d (x:xs) (m, n) c = x : update2d xs (m - 1, n) c

update3d :: [[[Char]]] -> (Int, Int, Int) -> Char -> [[[Char]]]
update3d (x:xs) (0, m, n) c = update2d x (m, n) c : xs
update3d (x:xs) (w, m, n) c = x : update3d xs (w - 1, m, n) c

update4d :: [[[[Char]]]] -> (Int, Int, Int, Int) -> Char -> [[[[Char]]]]
update4d (x:xs) (0, w, m, n) c = update3d x (w, m, n) c : xs
update4d (x:xs) (v, w, m, n) c = x : update4d xs (v - 1, w, m, n) c

computeNextGen_ :: [[[Char]]] -> [[[Char]]] -> [(Int, Int, Int)] -> [[[Char]]]
computeNextGen_ _ t [] = t
computeNextGen_ m t (x:xs) = computeNextGen_ m t' xs
  where
    u = nextGen m x
    t' = update3d t x u

computeNextGen :: [[[Char]]] -> [[[Char]]]
computeNextGen m = computeNextGen_ p t coords
  where
    p = prepare m
    ds = dims p
    coords = toCoords ds
    t = p

computeNextGen4d_ ::
     [[[[Char]]]] -> [[[[Char]]]] -> [(Int, Int, Int, Int)] -> [[[[Char]]]]
computeNextGen4d_ _ t [] = t
computeNextGen4d_ m t (x:xs) = computeNextGen4d_ m t' xs
  where
    u = nextGen4d m x
    t' = update4d t x u

computeNextGen4d :: [[[[Char]]]] -> [[[[Char]]]]
computeNextGen4d m = computeNextGen4d_ p t coords
  where
    p = prepare4d m
    ds = dims4d p
    coords = toCoords4d ds
    t = p

countActive :: [[[Char]]] -> Int
countActive m =
  sum $
  map
    (\x ->
       if x == '.'
         then 0
         else 1) $
  concat $ concat m

countActive4d :: [[[[Char]]]] -> Int
countActive4d = countActive . concat

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 0 _ x = x
applyNTimes n f x = applyNTimes (n - 1) f (f x)
