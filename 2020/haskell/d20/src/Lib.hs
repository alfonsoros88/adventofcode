module Lib
  ( toTile
  , solveArrangement
  , arrangementValue
  , toArrangement
  , Arrangement
  , assamble
  , findAllMonsters
  , countPixels
  ) where

import Control.Monad
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (find, intercalate, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace

data Pixel
  = Empty
  | Full
  deriving (Eq)

instance Show Pixel where
  show Full = "#"
  show Empty = "."

toPixel :: Char -> Pixel
toPixel c
  | c == '#' = Full
  | otherwise = Empty

fromPixel :: Pixel -> Char
fromPixel p =
  case p of
    Full -> '#'
    Empty -> '.'

data Tile =
  Tile
    { name :: Int
    , pixels :: [[Pixel]]
    }

instance Show Tile where
  show Tile {name, pixels} =
    "\nTile: " ++ show name ++ "\n" ++ intercalate "\n" pixels' ++ "\n"
    where
      pixels' = map fromPixel <$> pixels

instance Eq Tile where
  (==) t u = name t == name u

toTile :: String -> Tile
toTile input = Tile {pixels, name}
  where
    (h:pxs) = lines input
    name = read $ init (words h !! 1)
    pixels = map toPixel <$> pxs

-- | Rotates the tile counter clock-whise 90 degrees
rotate :: Tile -> Tile
rotate t@Tile {pixels} = t {pixels = p'}
  where
    p' = transpose (reverse <$> pixels)

rot :: [[Pixel]] -> [[Pixel]]
rot = transpose . fmap reverse

-- | Flips the tile vertically
flipTile :: Tile -> Tile
flipTile t@Tile {pixels} = t {pixels = reverse pixels}

flp :: [[Pixel]] -> [[Pixel]]
flp = reverse

transforms :: Tile -> [Tile]
transforms t =
  [ r $ f t
  | r <- [id, rotate, rotate . rotate, rotate . rotate . rotate]
  , f <- [id, flipTile]
  ]

-- | Returns the right border of the tile
rightBorder :: Tile -> [Pixel]
rightBorder Tile {pixels} = last <$> pixels

-- | Returns the left border of the tile
leftBorder :: Tile -> [Pixel]
leftBorder Tile {pixels} = head <$> pixels

-- | Returns the Tile's top border
topBorder :: Tile -> [Pixel]
topBorder Tile {pixels} = head pixels

-- | Returns the Tile's bottom border
bottomBorder :: Tile -> [Pixel]
bottomBorder Tile {pixels} = last pixels

borders :: Tile -> [[Pixel]]
borders t = [topBorder t, rightBorder t, bottomBorder t, leftBorder t]

-- | Returns True if the border matches with one of the borders of the tile
hasBorder :: [Pixel] -> Tile -> Bool
hasBorder p t = p `elem` bs || p' `elem` bs
  where
    bs = borders t
    p' = reverse p

matchLeft :: Tile -> Tile -> Bool
matchLeft a b = leftBorder a == rightBorder b

matchBottom :: Tile -> Tile -> Bool
matchBottom a b = bottomBorder a == topBorder b

data Arrangement =
  Arrangement
    { width :: Int
    , placed :: [Tile]
    , remaning :: [Tile]
    }

placeTile :: Arrangement -> [Arrangement]
placeTile a@Arrangement {width, placed, remaning}
  | row == 0 && col == 0 = initialSet remaning
  | row == 0 && col > 0 = place a <$> (remaning >>= filterTiles mleft)
  | row > 0 && col == 0 = place a <$> (remaning >>= filterTiles mbottom)
  | row > 0 && col > 0 =
    place a <$> (remaning >>= filterTiles (\x -> mleft x && mbottom x))
  where
    idx = length placed
    row = idx `div` width
    col = idx `rem` width
    mleft = matchLeft (head placed)
    mbottom = matchBottom (placed !! (width - 1))

filterTiles :: (Tile -> Bool) -> Tile -> [Tile]
filterTiles pred x = do
  x' <- transforms x
  guard $ pred x'
  return x'

place :: Arrangement -> Tile -> Arrangement
place a@Arrangement {placed, remaning} t =
  a {placed = placed', remaning = remaning'}
  where
    remaning' = filter (/= t) remaning
    placed' = t : placed

solveArrangement :: Arrangement -> [Arrangement]
solveArrangement a@Arrangement {remaning}
  | null remaning = [a]
  | otherwise = placeTile a >>= solveArrangement

initialSet :: [Tile] -> [Arrangement]
initialSet tiles = do
  x <- tiles
  x' <- transforms x
  let remaning = filter (/= x) tiles
  let width = floor . sqrt . fromIntegral $ length tiles
  return Arrangement {width, placed = [x'], remaning}

toArrangement :: [Tile] -> Arrangement
toArrangement tiles = Arrangement {width, placed = [], remaning = tiles}
  where
    width = floor . sqrt . fromIntegral $ length tiles

arrangementValue :: Arrangement -> Int
arrangementValue Arrangement {width, placed} = product $ name <$> [a, b, c, d]
  where
    a = head placed
    b = placed !! (width - 1)
    c = placed !! ((width - 1) * width)
    d = last placed

instance Show Arrangement where
  show Arrangement {placed} = show placed

----- part 2
removeBorders :: Tile -> [[Pixel]]
removeBorders Tile {pixels} = cut <$> cut pixels
  where
    cut = tail . init

joinTiles :: [[Pixel]] -> [[Pixel]] -> [[Pixel]]
joinTiles a b = ((++) <$> fst <*> snd) <$> zip a b

_assamble :: Int -> [Tile] -> [[Pixel]]
_assamble _ [] = []
_assamble w tiles = _assamble w rest ++ row'
  where
    (row, rest) = splitAt w tiles
    row' = foldl1 joinTiles (removeBorders <$> row)

newtype Pic =
  Pic [[Pixel]]

instance Show Pic where
  show (Pic pixels) = "\n" ++ intercalate "\n" chars ++ "\n"
    where
      chars = fmap (fmap fromPixel) pixels

assamble :: Arrangement -> Pic
assamble Arrangement {width, placed} = Pic $ _assamble width placed

monsterascii =
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   "]

monster :: [[Pixel]]
monster = fmap (fmap toPixel) monsterascii

monsterDim :: (Int, Int)
monsterDim = (length monster, length $ head monster)

searchCoords :: Pic -> [(Int, Int)]
searchCoords (Pic pixels) =
  [(x, y) | x <- [0 .. (n - mx)], y <- [0 .. (n - my)]]
        -- pictures are squared
  where
    n = length pixels
    (mx, my) = monsterDim

checkMonsterAt :: (Int, Int) -> Pic -> Bool
checkMonsterAt coord pic = isMonster $ subPic pic coord monsterDim

subPic :: Pic -> (Int, Int) -> (Int, Int) -> [[Pixel]]
subPic (Pic pixels) (x, y) (h, w) = crop_cols $ crop_rows pixels
  where
    crop_rows = take h . drop x
    crop_cols = fmap (take w . drop y)

isMonster :: [[Pixel]] -> Bool
isMonster pic = all pred $ zip (concat monster) (concat pic)
  where
    pred m =
      case m of
        (Full, Full) -> True
        (Full, Empty) -> False
        (Empty, _) -> True

emptyPic :: Int -> Pic
emptyPic n = Pic $ replicate n $ replicate n Empty

drawMonsterAt :: (Int, Int) -> Pic -> Pic
drawMonsterAt (x, y) (Pic pixels) = Pic $ top ++ result ++ bot
  where
    n = length pixels
    (h, w) = monsterDim
    (top, hs) = splitAt x pixels
    (orig, bot) = splitAt h hs
    monsterRows = padEmpty (y, n - w - y) <$> monster
    result = uncurry overrideRow <$> zip monsterRows orig

padEmpty :: (Int, Int) -> [Pixel] -> [Pixel]
padEmpty (before, after) pixels =
  replicate before Empty ++ pixels ++ replicate after Empty

overrideRow :: [Pixel] -> [Pixel] -> [Pixel]
overrideRow =
  zipWith
    (\a b ->
       if a == Full
         then Full
         else b)

mergePic :: Pic -> Pic -> Pic
mergePic (Pic a) (Pic b) = Pic $ uncurry overrideRow <$> zip a b

findAllMonsters :: Pic -> Pic
findAllMonsters pic@(Pic pixels) = foldl mergePic emp pics
    where
        n = length pixels
        emp = emptyPic n
        pics :: [Pic]
        pics = transformsPic pic >>= \p -> [findMonsters p]

findMonsters :: Pic -> Pic
findMonsters pic@(Pic pixels) = foldl mergePic emp pics
  where
    n = length pixels
    emp = emptyPic n
    pics = do
      coord <- searchCoords pic
      if checkMonsterAt coord pic
        then return (drawMonsterAt coord emp)
        else return emp

transformsPic :: Pic -> [Pic]
transformsPic (Pic pixels) =
  [ Pic $ r $ f pixels
  | r <- [id, rot, rot . rot, rot . rot . rot]
  , f <- [id, flip]
  ]
  where
    rot = transpose . fmap reverse
    flip = reverse

countPixels :: Pic -> Int
countPixels (Pic p) = foldl (\acc c -> if c == Full then acc + 1 else acc) 0 $ concat p
