{-# LANGUAGE DataKinds #-}

module CellArray where

import           Colour
import           Data.List.Index
import           Lib
import           Rainbow

-- data constructor for individual cells
data GameCell = GameCell
    { r     :: Int
    , g     :: Int
    , b     :: Int
    , alive :: Bool
    } deriving (Show)

-- data constructor for square game grid with size (dim x dim) with
-- all blank cells
data GameGrid = GameGrid
    { dim :: Int indexed (replicate dim (GameCell 0 0 0 False))
    }

-- grabs list of neighbouring cells' indices with edge wrapping
cellNeighbours :: GameGrid -> Int -> [Int]
cellNeighbours g n
        -- top left corner case
    | (n == 0) = [1, (x - 1), x, (x + 1), (x * 2 - 1), (x * (x - 1)), (x * (x - 1) + 1), (x * x - 1)]
        --top right corner case
    | n == (x - 1) = [0, (n - 1), (n + x), (n + x - 1), (x + 1), (x * x - 1), (x * x - 2), (x * (x - 1))]
        --bottom right corner case
    | n == (x * x - 1) =
        [0, (x - 1), (x - 2), (n - 1), (x * (x - 2)), (x * (x - 1)), (x * (x - 1) - 1), (x * (x - 1) - 2)]
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
        [(n + 1), (n - 1), ((n - x) - 1), (n - x), ((n - x) + 1), ((n mod x) - 1) (n mod x), ((n mod x) + 1)]
        --leftmost row case
    | (n mod x) == 0 =
        [(n - 1), (n + 1), (n - x), ((n - x) + 1), ((n + x) - 1), (n + x), ((n + x) + 1), (x + (2 * x) - 1)]
        --rightmost row case
    | (n mod x) == (x - 1) =
        [(n - 1), (n + 1), (n - (2 * x) + 1), ((n - x) - 1), (n - x), ((n - x) + 1), (n + x), ((n + x) - 1)]
        -- all others
    | otherwise = [(n + 1), (n - 1), ((n + x) - 1), (n + x), ((n + x) + 1), ((n - x) - 1), (n - x), ((n - x) + 1)]
  where
    x = g . dim

--counts live neighbours
liveNeighbours :: [Int] -> Int
newCell :: GameCell -> GameCell
newCell c = c

-- maps
nextGrid :: [GameCell] -> [GameCell]
nextGrid old = map (\x -> x) old

displayGrid :: [GameCell] -> String
displayGrid g = ""
