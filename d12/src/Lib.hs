module Lib
  ( Cmd
  , readCmd
  , step
  , newShip
  , manhattan
  , step2
  , Waypoint
  , newWaypoint
  ) where

data Cmd
  = N Int
  | E Int
  | S Int
  | W Int
  | F Int
  | R Int
  | L Int
  deriving (Show)

data Orientation
  = North
  | South
  | East
  | West
  deriving (Show)

data Ship =
  Ship Orientation (Int, Int)

newtype Waypoint =
  Waypoint (Int, Int)

readCmd :: String -> Cmd
readCmd (x:xs) =
  case x of
    'N' -> N (read xs :: Int)
    'S' -> S (read xs :: Int)
    'E' -> E (read xs :: Int)
    'W' -> W (read xs :: Int)
    'F' -> F (read xs :: Int)
    'R' -> R (read xs :: Int)
    'L' -> L (read xs :: Int)

newShip :: Ship
newShip = Ship East (0, 0)

newWaypoint :: Waypoint
newWaypoint = Waypoint (10, 1)

class Rotable a where
  left :: a -> a
  right :: a -> a

instance Rotable Orientation where
  right o =
    case o of
      North -> East
      East -> South
      South -> West
      West -> North
  left o =
    case o of
      North -> West
      West -> South
      South -> East
      East -> North

instance Rotable Waypoint where
  left (Waypoint (x, y)) = Waypoint (-y, x)
  right (Waypoint (x, y)) = Waypoint (y, -x)

step :: Ship -> Cmd -> Ship
step ship@(Ship o (x, y)) cmd =
  case cmd of
    N i -> Ship o (x, y + i)
    S i -> Ship o (x, y - i)
    E i -> Ship o (x + i, y)
    W i -> Ship o (x - i, y)
    F i -> forward ship i
    R i -> Ship (rotate (quot i 90) right o) (x, y)
    L i -> Ship (rotate (quot i 90) left o) (x, y)
  where
    rotate 0 dir o = o
    rotate n dir o = rotate (n - 1) dir (dir o)

forward :: Ship -> Int -> Ship
forward (Ship o (x, y)) n =
  case o of
    North -> Ship o (x, y + n)
    South -> Ship o (x, y - n)
    East -> Ship o (x + n, y)
    West -> Ship o (x - n, y)

advance :: (Ship, Waypoint) -> Int -> Ship
advance (Ship o (x, y), Waypoint (wx, wy)) v = Ship o (x', y')
  where
    x' = wx * v + x
    y' = wy * v + y

step2 :: (Ship, Waypoint) -> Cmd -> (Ship, Waypoint)
step2 (ship@(Ship o (x, y)), wp@(Waypoint (wx, wy))) cmd =
  case cmd of
    N i -> (ship, Waypoint (wx, wy + i))
    S i -> (ship, Waypoint (wx, wy - i))
    E i -> (ship, Waypoint (wx + i, wy))
    W i -> (ship, Waypoint (wx - i, wy))
    F i -> (advance (ship, wp) i, wp)
    R i -> (ship, rotate (quot i 90) right wp)
    L i -> (ship, rotate (quot i 90) left wp)
  where
    rotate 0 dir o = o
    rotate n dir o = rotate (n - 1) dir (dir o)

manhattan :: Ship -> Int
manhattan (Ship _ (x, y)) = abs x + abs y
