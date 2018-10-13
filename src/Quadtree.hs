module Quadtree where

data Quadtree t = Atom t |
  Quadtree {
    ul :: Quadtree t,
    ur :: Quadtree t,
    ll :: Quadtree t,
    lr :: Quadtree t
}