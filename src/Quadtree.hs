module Quadtree where

-- Define the Quadtree data type where each quadrant can be either another Quadtree or Dead or Alive
data Quadtree t
  = Quadtree { nw :: Quadtree t
             , ne :: Quadtree t
             , sw :: Quadtree t
             , se :: Quadtree t }
  | Dead
  | Alive

-- Define how to print the representation of the Quadtree in grid form
instance (Show a) => Show (Quadtree a) where
  show Dead = "-"
  show Alive = "#"
  show (Quadtree nw ne sw se) =
    foldr
      (++)
      []
      [ unlines $ zipWith (++) (lines $ show nw) (lines $ show ne)
      , unlines $ zipWith (++) (lines $ show sw) (lines $ show se)
      ]

-- Define accessors for the quadrants in a Quadtree
data SubQuadChunk = NW | NE | SW | SE

(#) :: Quadtree t -> SubQuadChunk -> Quadtree t
qt # NW = nw qt
qt # NE = ne qt
qt # SW = sw qt
qt # SE = se qt

-- Returns the depth of the Quadtree; depth of a cell of either Dead or Alive is 0
depth :: Quadtree t -> Int
depth (Quadtree nw ne sw se) = (1 + maximum (map depth [(nw), (ne), (sw), (se)]))
depth qt = 0

-- Produces Quadtree t from list of Quadtree t
qtFromList :: [Quadtree t] -> Quadtree t
qtFromList [nw, ne, sw, se] = Quadtree nw ne sw se

-- Get list of subtrees from Quadtree
qtToList :: (Quadtree t) -> [Quadtree t]
qtToList (Quadtree nw ne sw se) = [nw, ne, sw, se]

-- Parse a char into a Quadtree state of either Dead of Alive
parseChar :: Char -> Quadtree t
parseChar ch = case ch of
                    '#' -> Alive
                    '-' -> Dead

-- Parse the head of a string into a Quadtree state
parseCharStr :: [Char] -> Quadtree t
parseCharStr str = parseChar (head str)

-- From a string, derive the appropriate Quadtree representation of the grid space
qtFromString :: String -> Quadtree Char
qtFromString str | height > 2 = Quadtree nwTree neTree
                                         swTree seTree
                 | otherwise = Quadtree (parseCharStr (westChunks !! 0)) (parseCharStr (eastChunks !! 0))
                                        (parseCharStr (westChunks !! 1)) (parseCharStr (eastChunks !! 1))
  where rows = lines str
        height = length rows
        halfHeight = (height `div` 2)
        halvedLines = [splitAt halfHeight row | row <- rows]
        (westChunks, eastChunks) = (unzip halvedLines)
        (nwChunks, swChunks) = splitAt halfHeight westChunks
        (nwQuadrant, swQuadrant) = (unlines nwChunks, unlines swChunks)
        (neChunks, seChunks) = splitAt halfHeight eastChunks
        (neQuadrant, seQuadrant) = (unlines neChunks, unlines seChunks)
        nwTree = qtFromString nwQuadrant
        neTree = qtFromString neQuadrant
        swTree = qtFromString swQuadrant
        seTree = qtFromString seQuadrant

-- All functions below are used to help evolve a tree to the next generation. They create new Quadtrees in order to be
-- able to access a nodes neighbours so that the shouldLive function can be called correctly. This needs to be done
-- so that a node on the edge one of one Quadtree knows what neighbours it has in a different Quadtree.

-- Creates a node centered horizontally and vertically from a single Quadtree
centeredSubNode :: Quadtree t -> Quadtree t
centeredSubNode (Quadtree (Quadtree {se = newNW}) (Quadtree {sw = newNE})
                         (Quadtree {ne = newSW}) (Quadtree {nw = newSE}))
                        = Quadtree newNW newNE
                                   newSW newSE

-- Creates a node centered horizontally from 2 Quadtrees
centeredHorizontal (Quadtree  _ Quadtree {se = newNW}
                              _ Quadtree {ne = newSW})
                   (Quadtree Quadtree {sw = newNE} _
                             Quadtree {nw = newSE} _)
                  = Quadtree newNW newNE
                             newSW newSE

-- Creates a node centered vertically from 2 Quadtrees
centeredVertical (Quadtree _ _
                           Quadtree {se = newNW} Quadtree {sw = newNE})
                 (Quadtree Quadtree {ne = newSW} Quadtree {nw = newSE}
                           _ _)
                 = Quadtree newNW newNE
                            newSW newSE

-- All functions below are used to pad out a Quadtree before having evolve called on it. This is needed because evolve
-- requires two additional layers beyond the grid size in order to create the nodes required by the functions above.

-- Pads a Quadtree out with two additional layers
pad :: Quadtree t -> Quadtree t
pad qt = Quadtree (Quadtree emptyTree emptyTree emptyTree (nw qt))
                  (Quadtree emptyTree emptyTree (ne qt) emptyTree)
                  (Quadtree emptyTree (sw qt) emptyTree emptyTree)
                  (Quadtree (se qt) emptyTree emptyTree emptyTree)
          where dep = depth qt
                emptyTree = deadTreeFor (dep - 1)

-- Takes a number of levels and a Quadtree and pads that Quadtree by the number of levels
padBy :: (Eq t1, Num t1) => t1 -> Quadtree t2 -> Quadtree t2
padBy 0 qt = qt
padBy n qt = padBy (n-1) (pad qt)

-- Creates a Quadtree of only dead cells at the appropriate depth for the padding above
deadTreeFor :: (Integral i) => i -> Quadtree t
deadTreeFor 0 = Dead
deadTreeFor n = Quadtree (deadTreeFor $ n-1) (deadTreeFor $ n-1) (deadTreeFor $ n-1) (deadTreeFor $ n-1)