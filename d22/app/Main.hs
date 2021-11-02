module Main where

import Lib
import System.Environment
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let (p1, p2) = fromMaybe (emptyDeck, emptyDeck) $ parseDecks input
  print $ playGame (p1, p2)
  print $ recursiveGame (p1, p2)
