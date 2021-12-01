module Lib
  ( countDifferentAnswers
  , countCommonAnswers
  ) where

import qualified Data.Set as Set

countDifferentAnswers :: [[String]] -> Int
countDifferentAnswers ls = sum $ Set.size . Set.fromList <$> fmap concat ls

countCommonAnswers :: [[String]] -> Int
countCommonAnswers ls =
  sum $ Set.size . foldl1 Set.intersection <$> fmap (fmap Set.fromList) ls
