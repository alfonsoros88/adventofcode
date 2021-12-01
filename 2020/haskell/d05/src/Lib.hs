module Lib
  ( seatId
  ) where

fd :: Char -> Char -> Int -> Int -> String -> Maybe Int
fd _ _ _ _ [] = Nothing
fd l r lo hi (x:xs)
  | hi - lo == 2 =
    if | x == l -> Just lo
       | x == r -> Just (hi - 1)
       | otherwise -> Nothing
  | otherwise =
    if | x == l -> fd l r lo mi xs
       | x == r -> fd l r mi hi xs
       | otherwise -> Nothing
  where
    mi = lo + ((hi - lo) `div` 2)

row :: String -> Maybe Int
row = fd 'F' 'B' 0 128

col :: String -> Maybe Int
col = fd 'L' 'R' 0 8

seatId :: String -> Maybe Int
seatId s = case (r, c) of
    (Just x, Just y) -> Just (x * 8 + y)
    _ -> Nothing
    where
        r = row s
        c = col $ drop 7 s
