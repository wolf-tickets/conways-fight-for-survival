module CellArray where

import Data.List
import Data.Char
import Text.Show

-- data type and constructor for individual cells
data GameCell = GameCell
  { colour :: String
  , alive :: Bool
  } deriving (Show)

-- data type constructor for square game grid with size (dim x dim)
-- with all blank cells
data GameGrid = GameGrid
  { dim :: Int
  , cells :: [GameCell]
  } deriving (Show)

newGrid :: Int -> GameGrid
newGrid n = (GameGrid n (replicate (n * n) (GameCell "b" False)))

-- grabs list of neighbouring cells' indices with edge wrapping
cellNeighbours :: GameGrid -> Int -> [Int]
cellNeighbours g n
        -- top left corner case
  | (n == 0) =
    [ 1
    , (x - 1)
    , x
    , (x + 1)
    , (x * 2 - 1)
    , (x * (x - 1))
    , (x * (x - 1) + 1)
    , (x * x - 1)
    ]
        --top right corner case
  | n == (x - 1) =
    [ 0
    , (n - 1)
    , (n + x)
    , (n + x - 1)
    , (x + 1)
    , (x * x - 1)
    , (x * x - 2)
    , (x * (x - 1))
    ]
        --bottom right corner case
  | n == (x * x - 1) =
    [ 0
    , (x - 1)
    , (x - 2)
    , (n - 1)
    , (x * (x - 2))
    , (x * (x - 1))
    , (x * (x - 1) - 1)
    , (x * (x - 1) - 2)
    ]
        --bottom left corner case
  | n == (x * (x - 1)) =
    [ 0
    , 1
    , (x - 1)
    , (x * (x - 2))
    , (x * (x - 2) + 1)
    , (x * (x - 1) - 1)
    , (x * (x - 1))
    , (x * (x - 1) + 1)
    , ((x * x) - 1)
    ]
        --top row case
  | n < x =
    [ (n + 1)
    , (n - 1)
    , (n + x - 1)
    , (n + x)
    , (n + x + 1)
    , (x * (x - 1) + n - 1)
    , (x * (x - 1) + n)
    , (x * (x - 1) + n + 1)
    ]
        --bottom row case
  | n >= (x * (x - 1)) =
    [ (n + 1)
    , (n - 1)
    , ((n - x) - 1)
    , (n - x)
    , ((n - x) + 1)
    , ((mod n x) - 1)
    , (mod n x)
    , ((mod n x) + 1)
    ]
        --leftmost row case
  | (mod n x) == 0 =
    [ (n - 1)
    , (n + 1)
    , (n - x)
    , ((n - x) + 1)
    , ((n + x) - 1)
    , (n + x)
    , ((n + x) + 1)
    , (x + (2 * x) - 1)
    ]
        --rightmost row case
  | (mod n x) == (x - 1) =
    [ (n - 1)
    , (n + 1)
    , (n - (2 * x) + 1)
    , ((n - x) - 1)
    , (n - x)
    , ((n - x) + 1)
    , (n + x)
    , ((n + x) - 1)
    ]
        -- all others
  | otherwise =
    [ (n + 1)
    , (n - 1)
    , ((n + x) - 1)
    , (n + x)
    , ((n + x) + 1)
    , ((n - x) - 1)
    , (n - x)
    , ((n - x) + 1)
    ]
  where
    x = dim g

-- helper function extracts live neighbour cells
liveNeighbours :: GameGrid -> [Int] -> [GameCell]
liveNeighbours g n = [cells g !! x | x <- n]

nextCell :: GameGrid -> Int -> GameCell
nextCell g c
  | (alive cell) && (n == 3 || n == 2) =
    GameCell (colourSum (liveNeighbours g (cellNeighbours g c))) True
  | (not (alive cell)) && (n == 3) =
    GameCell (colourSum (liveNeighbours g (cellNeighbours g c))) True
  | otherwise = GameCell "x" False
  where
    n = length (cellNeighbours g c)
    cell = (cells g !! c)

--neighbourSum :: [GameCell] -> GameCell -> GameCell
--neighbourSum cellList oldCell
--  | alive oldCell && (n == 3 || n == 2) = GameCell (colourSum cellList) True
--  | (not (alive (g !! c))) && (n == 3) = GameCell (colourSum cellList) True
--  | otherwise = GameCell 'x' False
--  where
--    n = length (cellList)

colourSum :: [GameCell] -> String
colourSum cellList
  | ((red > blue) && (red > green)) = "r"
  | ((green > red) && (green > blue)) = "g"
  | ((blue > red) && (blue > green)) = "b"
  | otherwise = "w"
  where
    red = (length (filter (\c -> colour c == "r") cellList))
    green = (length (filter (\c -> colour c == "g") cellList))
    blue = (length (filter (\c -> colour c == "b") cellList))
--displayGrid :: [GameCell] -> String
--displayGrid g = ""
