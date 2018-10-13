module Quadtree where

data Quadtree t = Cell t |
  Quadtree {
    ul :: Quadtree t,
    ur :: Quadtree t,
    ll :: Quadtree t,
    lr :: Quadtree t
}

square :: Quadtree [Char]
square = Quadtree (Cell "ul") (Cell "ur") (Cell "ll") (Cell "lr")
