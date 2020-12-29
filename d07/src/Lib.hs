module Lib
  ( Entries
  , parseEntries
  , countHowManyCanHold
  , countBagsInside
  ) where

import Data.List.Split
import qualified Data.Map.Strict as Map

readBagQuantity :: String -> (Int, String)
readBagQuantity input = (number, bag)
  where
    (n:ns) = words input
    number = read n :: Int
    bag = unwords $ take 2 ns

splitQuantities :: String -> [(Int, String)]
splitQuantities input = readBagQuantity <$> splitOn ", " input

readEntry :: String -> (String, [(Int, String)])
readEntry input = (name, content)
  where
    (name:quantitites:_) = splitOn " bags contain " input
    content =
      case quantitites of
        "no other bags." -> []
        other -> splitQuantities other

type Entries = Map.Map String [(Int, String)]

parseEntries :: String -> Entries
parseEntries input = Map.fromList $ readEntry <$> lines input

bags :: Entries -> String -> [String]
bags entries x =
  case Map.lookup x entries of
    Nothing -> []
    Just ls -> f <$> ls
  where
    f (_, x) = x

canHold :: Entries -> String -> String -> Bool
canHold m x y = elem y bs || any (\v -> canHold m v y) bs
  where
    bs = bags m x

countHowManyCanHold :: Entries -> String -> Int
countHowManyCanHold m x = length . filter (\v -> canHold m v x) $ bgs
    where
        bgs = Map.keys m

countBagsInside :: Entries -> String -> Int
countBagsInside m x = count
    where
        bgs = Map.lookup x m
        count = case bgs of
            Nothing -> 1
            Just ls -> sum $ (\(w, t) -> w + w * countBagsInside m t) <$> ls


