module CellArray where
import Lib
import Rainbow
import Colour
import Data.List.Index

--Constants

dim = 10
iterations = 1000

InitGrid = [(Cell 0 0 0 0 O) .. (Cell 0 0 0 (dim * dim - 1) 0)]


data Life = O | X deriving (Show, Radiant)

data GameCell = GameCell
    { r   :: Int
    , g :: Int
    , b  :: Int
    , alive :: Life
    } deriving (Show)

data

data GameGrid = GameGrid indexed [GameCell]



-- grabs list of neighbouring cells' indices
cellNeighbours :: GameGrid -> Int -> [Int]
cellNeighbours g n =
	[\g n -> g !! [(n + 1), (n - 1), (n + dim), (n - dim), (n + dim - 1), (n + dim + 1), ((n - dim) - 1), ((n - dim) + 1)]]

[(c.xpos - 1) (c.xpos + 1)

newCell :: GameCell -> GameCell
newCell c = c

-- maps
nextGrid :: [GameCell] -> [GameCell]
nextGrid old = map (\x -> x) old

displayGrid :: [GameCell] -> String
displayGrid g = ""

