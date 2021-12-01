module Main where

import Lib
import Control.Monad
import Debug.Trace

main :: IO ()
main = do
  let cups = read "364289715" :: Cups
  print $ part1Score $ moven 100 cups

  let bigCups = toBigCups cups
  print $ part2score $ foldr (\i c -> moveBig c) bigCups [1..10000000]


