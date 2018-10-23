module Lib where

import Quadtree
import CellArray

-- dead = Dead
-- Alive = Alive

--alive = Cell 1


--shouldLive cell neighbours
--shouldLive :: Quadtree Int -> Int -> Quadtree Int
shouldLive :: Quadtree Char -> Int -> Quadtree Char
shouldLive cell numNeighbours =
  if numNeighbours > 3 || numNeighbours < 2
    then Dead
    else (case numNeighbours of
               3 -> Alive
               _ -> cell)

--sumCells = foldr (\ (Cell v) s -> v + s ) 0
sumCells = foldr (\ v s -> s + (case v of
                                        Dead -> 0
                                        Alive -> 1)) 0

-- Evolve a quadtree at level 2
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

--evolve grid
evolve :: Quadtree Char -> Quadtree Char
evolve grid | depth grid == 2 = evolveLevel2 grid
            | otherwise       = Quadtree 
                                        (evolve $ Quadtree q00 q01 q10 q11)
                                        (evolve $ Quadtree q01 q02 q11 q12)
                                        (evolve $ Quadtree q10 q11 q20 q21)
                                        (evolve $ Quadtree q11 q12 q21 q22)
            where
                q00 = centeredSubNode (nw grid)
                q01 = centeredHorizontal (nw grid) (ne grid)
                q02 = centeredSubNode (ne grid)
                q10 = centeredVertical (nw grid) (sw grid)
                q11 = centeredSubNode (centeredSubNode grid)
                q12 = centeredVertical (ne grid) (se grid)
                q20 = centeredSubNode (sw grid)
                q21 = centeredHorizontal (sw grid) (se grid)
                q22 = centeredSubNode (se grid)

--evolveBy generations qt
evolveBy 0 qt = qt
evolveBy generations qt = evolveBy (generations - 1) (evolve $ pad $ qt)

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

