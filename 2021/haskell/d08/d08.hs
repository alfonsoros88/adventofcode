module Main where
import Data.List (group, find, sort)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromJust)

isOne :: String -> Bool
isOne = (== 2) . length

isFour :: String -> Bool
isFour = (== 4) . length

isSeven :: String -> Bool
isSeven = (== 3) . length

isEight :: String -> Bool
isEight = (== 7) . length

type Entry = [String]

parseEntries :: String -> [Entry]
parseEntries = map words . lines

display :: Entry -> [String]
display = take 4 . reverse

part1 =
  length .
  filter (\x -> isOne x || isFour x || isSeven x || isEight x) .
  concatMap display . parseEntries

-- part2

signals :: Entry -> [String]
signals = take 10

countLetters :: [String] -> [(Char, Int)]
countLetters = map (\x -> (head x, length x)) . group . sort . concat

--   0:      1:      2:      3:      4:
--  aaaa    ....    aaaa    aaaa    ....
-- b    c  .    c  .    c  .    c  b    c
-- b    c  .    c  .    c  .    c  b    c
--  ....    ....    dddd    dddd    dddd
-- e    f  .    f  e    .  .    f  .    f
-- e    f  .    f  e    .  .    f  .    f
--  gggg    ....    gggg    gggg    ....
--
--   5:      6:      7:      8:      9:
--  aaaa    aaaa    aaaa    aaaa    aaaa
-- b    .  b    .  .    c  b    c  b    c
-- b    .  b    .  .    c  b    c  b    c
--  dddd    dddd    ....    dddd    dddd
-- .    f  e    f  .    f  e    f  .    f
-- .    f  e    f  .    f  e    f  .    f
--  gggg    gggg    ....    gggg    gggg
--
-- letterr counts:
-- a: 8
-- b: 6
-- c: 8
-- d: 7
-- e: 4
-- f: 9
-- g: 7
segmentMap :: Entry -> HashMap String Int
segmentMap entry =
  let sig = signals entry
      letters = countLetters sig
      b = fst . fromJust $ find ((== 6) . snd) letters
      e = fst . fromJust $ find ((== 4) . snd) letters
      f = fst . fromJust $ find ((== 9) . snd) letters
      one = fromJust $ find isOne sig
      four = fromJust $ find isFour sig
      seven = fromJust $ find isSeven sig
      a = head [x | x <- seven, x `notElem` one]
      c = head [x | x <- one, x /= f]
      d = head [x | x <- four, x `notElem` b : one]
      g = head [x | x <- fst <$> filter ((== 7) . snd) letters, x /= d]
   in Map.fromList
        [ (sort [a, b, c, e, f, g], 0)
        , (sort [c, f], 1)
        , (sort [a, c, d, e, g], 2)
        , (sort [a, c, d, f, g], 3)
        , (sort [b, d, c, f], 4)
        , (sort [a, b, d, f, g], 5)
        , (sort [a, b, d, e, f, g], 6)
        , (sort [a, c, f], 7)
        , (sort [a, b, c, d, e, f, g], 8)
        , (sort [a, b, c, d, f, g], 9)
        ]

decode :: Entry -> Int
decode entry = let
    m = segmentMap entry
    digits = sort <$> display entry
    in foldl (\acc c -> acc * 10 + c) 0 $ reverse $ map (m Map.!) digits

part2 = sum . map decode . parseEntries

main :: IO ()
main = do
  input <- readFile "inputs/d08.txt"
  print $ part1 input
  print $ part2 input
