module Lib
  ( parsePrg
  , run
  , runCorrect
  ) where

import Data.List

data Cmd
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Show)

type Idx = Int

type Acc = Int

data Prg =
  Prg [Cmd] Idx Acc
  deriving (Show)

parseCmd :: String -> Cmd
parseCmd str
  | "acc" `isPrefixOf` str = Acc int
  | "jmp" `isPrefixOf` str = Jmp int
  | "nop" `isPrefixOf` str = Nop int
  where
    intStr = last $ words str
    int =
      case intStr of
        '+':xs -> read xs :: Int
        '-':xs -> -read xs :: Int

parsePrg :: String -> [Cmd]
parsePrg input = parseCmd <$> lines input

step :: Prg -> Prg
step (Prg cmds i a) = Prg cmds idx acc
  where
    cmd = cmds !! i
    (idx, acc) =
      case cmd of
        Acc x -> (i + 1, a + x)
        Jmp x -> (i + x, a)
        Nop _ -> (i + 1, a)

type History = [Bool]

run :: [Cmd] -> Acc
run cmds = fst $ run_ (Prg cmds 0 0) hist
  where
    hist = replicate (length cmds) False

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n x ls = pre ++ x : tail post
  where
    (pre, post) = splitAt n ls

data Termination
  = Failed
  | Success
  deriving (Show)

run_ :: Prg -> History -> (Acc, Termination)
run_ prg@(Prg cmds i a) hist
  | i == length cmds = (a, Success)
  | i > length cmds = (a, Failed)
  | hist !! i = (a, Failed)
  | otherwise = run_ newPrg newHist
  where
    newPrg = step prg
    newHist = replaceNth i True hist

swapCurrentJmpNop :: Prg -> Prg
swapCurrentJmpNop (Prg cmds i a) = Prg newCmds i a
  where
    newCmds =
      case cmds !! i of
        Jmp x -> replaceNth i (Nop x) cmds
        Nop x -> replaceNth i (Jmp x) cmds
        _ -> cmds

searchFaulty :: Prg -> History -> Acc
searchFaulty prg hist =
  case run_ (swapCurrentJmpNop prg) hist of
    (a, Success) -> a
    (a, Failed) -> searchFaulty nextPrg hist
  where
    nextPrg = step prg

runCorrect :: [Cmd] -> Acc
runCorrect cmds = searchFaulty (Prg cmds 0 0) hist
  where
    hist = replicate (length cmds) False
