module Main where

-- Four directins where the submarine can move
data SubmarineCommand
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show, Eq)

-- Read a submarine command from a string
-- The string must be in the format "forward x" or "down x" or "up x"
instance Read SubmarineCommand where
  readsPrec _ s =
    case words s of
      ["forward", x] -> [(Forward (read x), "")]
      ["down", x] -> [(Down (read x), "")]
      ["up", x] -> [(Up (read x), "")]
      _ -> []

-- Read a list of submarine commands from a the lines of a string
-- The string must be in the format "forward x\ndown x\nup x"
readSubmarineCommands :: String -> [SubmarineCommand]
readSubmarineCommands = map read . lines

data Coord =
  Coord
    { pos :: Int
    , depth :: Int
    , aim :: Int
    }
  deriving (Show, Eq)

-- Given a Coord and a SubmarineCommand, return the new Coord
move :: Coord -> SubmarineCommand -> Coord
move (Coord pos depth aim) command =
  case command of
    Forward n -> Coord (pos + n) depth aim
    Down n -> Coord pos (depth + n) aim
    Up n -> Coord pos (depth - n) aim

-- Return the Coord value correspoding as its position times its depth
value :: Coord -> Int
value (Coord pos depth _) = pos * depth

-- Calculate the final position of the submarine and return its value
part1 = value . foldl move (Coord 0 0 0) . readSubmarineCommands

-- part2
correctedMove :: Coord -> SubmarineCommand -> Coord
correctedMove (Coord pos depth aim) command =
  case command of
    Forward n -> Coord (pos + n) (depth + (aim * n)) aim
    Down n -> Coord pos depth (aim + n)
    Up n -> Coord pos depth (aim - n)

part2 = value . foldl correctedMove (Coord 0 0 0) . readSubmarineCommands

main :: IO ()
main = do
  input <- readFile "inputs/d02.txt"
  print $ part1 input
  print $ part2 input
