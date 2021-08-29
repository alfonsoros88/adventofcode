module Main where

import           Lib
import           System.Environment

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List           (intercalate)

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let items = fmap readIngredients (lines input)
  let count = ingredientsCount $ fst <$> items
  let (withoutAllergens, withAllergens) = separateIngredients items
  print $ sum $ (HashMap.!) count <$> withoutAllergens
  print $ intercalate "," withAllergens
