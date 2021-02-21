module Lib
  ( Parser
  , fromRules
  , consume
  , match
  ) where

import Data.Char (isDigit)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as Map
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)

data Parser
  = Lit String
  | Seq Parser Parser
  | Or Parser Parser
  | NoOp
  deriving (Show)

data Rule
  = Literal String
  | SeqR [Int]
  | OrR ([Int], [Int])

fromWords :: [Int] -> [String] -> Rule
fromWords seq [] = SeqR seq
fromWords seq (x:xs)
  | all isDigit x = fromWords (seq ++ [read x]) xs
  | x == "|" = OrR (seq, read <$> xs)
  | otherwise = Literal (init (tail x))

fromString :: String -> (Int, Rule)
fromString input = (id, rule)
  where
    (h:body) = words input
    id = read (init h)
    rule = fromWords [] body

toRuleMap :: [String] -> HashMap Int Rule
toRuleMap rules = Map.fromList (fromString <$> rules)

toParser :: HashMap Int Rule -> Int -> Parser
toParser rules id =
  case Map.lookup id rules of
    Just (Literal s) -> Lit s
    Just (SeqR xs) -> toSeq xs
    Just (OrR (xs, ys)) -> Or (toSeq xs) (toSeq ys)
  where
    toSeq = foldl (\p i -> Seq p (toParser rules i)) NoOp

fromRules :: [String] -> Parser
fromRules rules = toParser (toRuleMap rules) 0

consume :: Parser -> String -> Maybe String
consume p input =
  case p of
    NoOp -> Just input
    Lit s -> stripPrefix s input
    Seq p q -> consume p input >>= consume q
    Or p q -> consume p input <> consume q input

-- part 2
data Result
  = Base (Maybe String)
  | One Result
  | Two (Result, Result)

seqOp :: Parser -> Result -> Result
seqOp p r =
  case r of
    Base (Just s) -> consume2 p s
    Base Nothing -> Base Nothing
    One x -> seqOp p x
    Two (x, y) -> Two (seqOp p x, seqOp p y)

consume2 :: Parser -> String -> Result
consume2 p input =
  case p of
    NoOp -> Base (Just input)
    Lit s -> Base (stripPrefix s input)
    Seq p q -> One (seqOp q $ consume2 p input)
    Or p q -> Two (consume2 p input, consume2 q input)

isMatch :: Result -> Bool
isMatch r =
  case r of
    Base x -> x == Just ""
    One x -> isMatch x
    Two (x, y) -> isMatch x || isMatch y

match :: Parser -> String -> Bool
match p = isMatch . consume2 p
