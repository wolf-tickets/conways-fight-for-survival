module StrinOfife where

import Data.List
import Data.Char
import Text.Show
import Rainbow
import Graphics.Gloss

-- data type constructor for square game grid with size (dim x dim)
-- with all blank cells:
data GameGrid = GameGrid
  { d :: Int
  , cells :: Seq Char
  } deriving (Show)

newGrid :: Int -> GameGrid
newGrid n = (GameGrid n (replicate (n * n) 'b'))

randGrid :: GameGrid
randGrid = (GameGrid 10 ['d', 'd', 'd', 'g', 'd', 'b', 'b', 'd', 'y', 'd', 'd', 'd', 'd', 'c', 'd', 'r', 'r', 'r', 'd', 'd', 'c', 'b', 'b', 'r','d', 'd', 'd', 'g', 'd', 'b', 'b', 'd', 'y', 'd', 'd', 'd', 'd', 'c', 'd', 'r', 'r', 'r', 'd', 'd', 'c', 'b', 'b', 'r', 'd', 'd', 'd', 'g', 'd', 'b', 'b', 'd', 'y', 'd', 'd', 'd', 'd', 'c', 'd', 'r', 'r', 'r', 'd', 'd', 'c', 'b', 'b', 'r','d', 'd', 'd', 'g', 'd', 'b', 'b', 'd', 'y', 'd', 'd', 'd', 'd', 'c', 'd', 'r', 'r', 'r', 'd', 'd', 'c', 'b', 'b', 'r', 'r' , 'r', 'r', 'r'])


-- compute neighbours in advance, store as tuple of an Int and a [Char]
type cellNeighbours :: (Index :: Inr, Neighbours :: [Char]) 
cellNeighbours cn = 
        -- top left corner case
      (0, 
      [1
    , (x - 1)
    , x
    , (x + 1)
    , (x * 2 - 1)
    , (x * (x - 1))
    , (x * (x - 1) + 1)
    , (x * x - 1)
    ]
    )
    --top right corner case
    (
    (x - 1), 
    [ 0
    , (n - 1)
    , (n + x)
    , (n + x - 1)
    , (x + 1)
    , (x * x - 1)
    , (x * x - 2)
    , (x * (x - 1))
    ])
        --bottom right corner case
 ((x * x - 1), 
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
    x = d grid

-- helper function extracts live neighbour cells
liveNeighbours :: GameGrid -> [Int] -> [Char]
liveNeighbours grid indexes = [(cells grid) !! n | n <- indexes]


--determines the colour of the cell at the given index in the next iteration
nextCell :: GameGrid -> Int -> Char
nextCell grid index
  | (not (cell == 'd')) && (n == 3 || n == 2) =
      (colourSum (liveNeighbours grid (cellNeighbours grid index)))
  | (n == 3) =
    (colourSum (liveNeighbours grid (cellNeighbours grid index))) 
  | otherwise = 'd'
  where
    n = length (cellNeighbours grid index)
    cell = (cells grid !! index)

-- figure out the colour of the resulting cell - 'd' is dead (black, or background colour
-- 'w' for a 3- way tie ("white"), rgb,
-- 'p' for an r g tie (r,g,w), 'c' for a green blue tie (cyan), red-green = 'y'
colourSum :: [Char] -> Char
colourSum cellList
  | ((red > blue) && (red > green)) = 'r'
  | ((green > red) && (green > blue)) = 'g'
  | ((blue > red) && (blue > green)) = 'b'
  | ((blue == green)) = 'c'
  | ((blue == red)) = 'p'
  | ((red == green)) = 'y'
  | otherwise = 'w'
  where
    red = ((length(filter (\c -> (c =='r')) cellList)) + (div (length (filter (\c -> (c == 'p'|| c == 'y')) cellList)) 2)) 
    green = ((length(filter (\c -> (c == 'g')) cellList)) + (div (length (filter (\c -> (c == 'c'|| c =='y')) cellList)) 2))
    blue = ((length(filter (\c -> (c == 'b')) cellList)) + (div (length (filter (\c -> (c == 'c'||c =='p')) cellList)) 2))

nextGrid :: GameGrid -> GameGrid
nextGrid old = GameGrid (d old) [(nextCell old x) | x <- [0..(((d old) * (d old))-1)]]
