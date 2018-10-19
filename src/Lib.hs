module Lib where

import Quadtree

dead = Cell ' '
alive = Cell '#'

--alive = Cell 1


--shouldLive cell neighbours
--shouldLive :: Quadtree Int -> Int -> Quadtree Int
shouldLive :: Quadtree Char -> Int -> Quadtree Char
shouldLive cell numNeighbours =
  if numNeighbours > 3 || numNeighbours < 2
    then dead
    else (case numNeighbours of
               3 -> alive
               _ -> cell)

--sumCells = foldr (\ (Cell v) s -> v + s ) 0
sumCells = foldr (\ (Cell v) s -> s + (if v == ' ' then 0 else 1)) 0

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
