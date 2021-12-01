module Main where

import Lib
import System.Environment
import Text.Regex.Posix

parseLine :: String -> (Policy, String)
parseLine s = result $ getList match
  where
    match =
      s =~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" :: ( String
                                                    , String
                                                    , String
                                                    , [String])
    getList (_, _, _, m) = m
    toRange l h = Range {low = (read l), high = (read h)}
    toPolicy l h c = Policy {range = (toRange l h), letter = c}
    result (l:h:c:p:xs) = (toPolicy l h (head c), p)

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  input <- readFile path
  let policiesAndPasswords = map parseLine . lines $ input
  let validFirstPolicy = map (uncurry complies) $ policiesAndPasswords
  let validSecondPolicy = map (uncurry compliesUpdatedPolicy) $ policiesAndPasswords
  let validCount = foldl (\acc p -> case p of {True -> acc + 1; False -> acc}) 0
  let part1Valid = validCount validFirstPolicy
  let part2Valid = validCount validSecondPolicy
  putStrLn $ "part one answer: " ++ show part1Valid
  putStrLn $ "part two answer: " ++ show part2Valid
