module Lib
  ( tokenize
  , eval
  , resolveSum
  ) where

import Data.Char (isDigit)

data Token
  = Sum
  | Prod
  | OpenB
  | CloseB
  | Number Int
  deriving (Show)

tokenize :: [Char] -> [Token]
tokenize [] = []
tokenize ls@(x:xs) =
  case x of
    '+' -> Sum : tokenize xs
    '*' -> Prod : tokenize xs
    ' ' -> tokenize xs
    '(' -> OpenB : tokenize xs
    ')' -> CloseB : tokenize xs
    _
      | isDigit x -> Number v : tokenize rest
      | otherwise -> []
  where
    (ds, rest) = span isDigit ls
    v = read ds :: Int

data Operator
  = SumOp
  | ProdOp
  | NoOp

evalOp :: Operator -> Int -> Int -> Int
evalOp op a b =
  case op of
    SumOp -> a + b
    ProdOp -> a * b
    NoOp -> b

evalo :: (Int -> Int) -> [Token] -> (Int, [Token])
evalo op (x:xs) =
  case x of
    Number n -> evaln (op n) xs
    OpenB ->
      let (b, rest) = evalo (evalOp NoOp 0) xs
       in evaln (op b) rest

evaln :: Int -> [Token] -> (Int, [Token])
evaln n [] = (n, [])
evaln n (x:xs) =
  case x of
    Sum -> evalo (evalOp SumOp n) xs
    Prod -> evalo (evalOp ProdOp n) xs
    CloseB -> (n, xs)

eval :: [Token] -> Int
eval = fst . evalo (evalOp NoOp 0)

spanSubExp :: Int -> [Token] -> ([Token], [Token])
spanSubExp n [] = ([], [])
spanSubExp n (t:ts) =
  case t of
    CloseB ->
      if n == 0
        then ([], ts)
        else (CloseB : xs, ys)
      where (xs, ys) = spanSubExp (n - 1) ts
    OpenB -> (OpenB : xs, ys)
      where (xs, ys) = spanSubExp (n + 1) ts
    _ -> (t : xs, ys)
      where (xs, ys) = spanSubExp n ts

resolveSum :: [Token] -> [Token]
resolveSum [] = []
resolveSum (Number a:Sum:Number b:rest) = resolveSum (Number (a + b) : rest)
resolveSum (Number a:Sum:OpenB:rest) = resolveSum (Number (a + b) : xs)
  where
    (exp, xs) = spanSubExp 0 rest
    b = eval (resolveSum exp)
resolveSum (OpenB:ts) = resolveSum (Number a : xs)
  where
    (exp, xs) = spanSubExp 0 ts
    a = eval (resolveSum exp)
resolveSum (t:ts) = t : resolveSum ts
