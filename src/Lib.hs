module Lib where

import Quadtree

-- Calculates whether a cell should be alive or dead for the next generation
-- cell: the cell being evolved to the next generation
-- numNeighbours: the number of neighbours that cell has
shouldLive :: Quadtree Char -> Int -> Quadtree Char
shouldLive cell numNeighbours =
  if numNeighbours > 3 || numNeighbours < 2
    then Dead
    else (case numNeighbours of
               3 -> Alive
               _ -> cell)

-- Returns the number of neighbouring cells that are alive
sumCells :: [Quadtree t] -> Int
sumCells = foldr (\ v s -> s + (case v of
                                        Dead -> 0
                                        Alive -> 1)) 0

-- Takes a Quadtree of level 2 and returns the next generation
-- qtNW: the NW quadrant of the original Quadtree
-- qtNE: the NE quadrant of the original Quadtree
-- qtSW: the SW quadrant of the original Quadtree
-- qtSE: the SE quadrant of the original Quadtree
evolveLevel2 :: Quadtree Char -> Quadtree Char
evolveLevel2 (Quadtree qtNW qtNE qtSW qtSE) =
  Quadtree
    (shouldLive (se qtNW)  (sumCells [
    nw qtNW, ne qtNW, nw qtNE,
    sw qtNW,          sw qtNE,
    nw qtSW, ne qtSW, nw qtSE
    ]))
    (shouldLive (sw qtNE) (sumCells [
    ne qtNW, nw qtNE, ne qtNE,
    se qtNW,          se qtNE,
    ne qtSW, nw qtSE, ne qtSE
    ]))
    (shouldLive (ne qtSW) (sumCells [
    sw qtNW, se qtNW, sw qtNE,
    nw qtSW,          nw qtSE,
    sw qtSW, se qtSW, sw qtSE
    ]))
    (shouldLive (nw qtSE) (sumCells [
    se qtNW, sw qtNE, se qtNE,
    ne qtSW,          ne qtSE,
    se qtSW, sw qtSE, se qtSE
    ]))

-- Takes a grid of Quadtrees and evolves it to the next generation
-- grid: the grid of Quadtrees to evolve
evolve :: Quadtree Char -> Quadtree Char
evolve grid | depth grid == 2 = evolveLevel2 grid                               -- if depth 2, call evolveLevel2
            | otherwise       = Quadtree                                        -- else, recursively call evolve
                                        (evolve $ Quadtree q00 q01 q10 q11)
                                        (evolve $ Quadtree q01 q02 q11 q12)
                                        (evolve $ Quadtree q10 q11 q20 q21)
                                        (evolve $ Quadtree q11 q12 q21 q22)
            where                                                               -- split up into component parts
                q00 = centeredSubNode (nw grid)
                q01 = centeredHorizontal (nw grid) (ne grid)
                q02 = centeredSubNode (ne grid)
                q10 = centeredVertical (nw grid) (sw grid)
                q11 = centeredSubNode (centeredSubNode grid)
                q12 = centeredVertical (ne grid) (se grid)
                q20 = centeredSubNode (sw grid)
                q21 = centeredHorizontal (sw grid) (se grid)
                q22 = centeredSubNode (se grid)

-- Helper functions/variables for testing purposes
alive1 = Quadtree Alive Alive Alive Alive
dead1 = Quadtree Dead Dead Dead Dead

dead2 = Quadtree dead1 dead1 dead1 dead1

nw1 = Quadtree Dead Dead Dead Alive
ne1 = Quadtree Dead Dead Dead Dead
sw1 = Quadtree Dead Dead Alive Alive
se1 = Quadtree Alive Dead Alive Dead

nw2 = Quadtree dead1 dead1 dead1 nw1
ne2 = Quadtree dead1 dead1 ne1 dead1
sw2 = Quadtree dead1 sw1 dead1 dead1
se2 = Quadtree se1 dead1 dead1 dead1

nw3 = Quadtree dead2 dead2 dead2 nw2
ne3 = Quadtree dead2 dead2 ne2 dead2
sw3 = Quadtree dead2 sw2 dead2 dead2
se3 = Quadtree se2 dead2 dead2 dead2

gliderGrid = Quadtree    nw3 ne3
                         sw3 se3

