module CellArray where
import Data.Colour.SRGB


size = 100
iterations = 1000
--InitGrid = [(Cell 0 0 0 0 False) .. (Cell 0 0 0 99 False)]

  -- R G B xpos ypos alive2
data Life = O | X deriving (Show)

data GameCell = GameCell
    { red   :: Int
    , green :: Int
    , blue  :: Int
    , xpos  :: Int
    , ypos :: Int
    , alive :: Life
    } deriving (Show)


main :: IO ()
main =  print $ show $ 1 + 1

-- initialize gui

-- allow players to place cells

-- grabs list of neighbouring cells
neighbours :: GameCell -> [GameCell]
neighbours c = [c]

newCell :: GameCell -> GameCell
newCell c = c

-- maps
nextGrid :: [GameCell] -> [GameCell]
nextGrid old = map (\x -> x) old

displayGrid :: [GameCell] -> String
displayGrid g = ""

