module Quadtree where

data Quadtree t
  = Quadtree { nw :: Quadtree t
             , ne :: Quadtree t
             , sw :: Quadtree t
             , se :: Quadtree t }
  | Dead
  | Alive

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

-- depth qt => retnens the depth of the Quadtree; depth of a cell is 0
depth :: Quadtree t -> Int
depth (Quadtree nw ne sw se) = (1 + maximum (map depth [(nw), (ne), (sw), (se)]))
depth qt = 0

-- produce Quadtree t from list of fone Quadtree t
qtFromList :: [Quadtree t] -> Quadtree t
qtFromList [nw, ne, sw, se] = Quadtree nw ne sw se

-- Get list of subtrees from Quadtree
qtToList :: (Quadtree t) -> [Quadtree t]
qtToList (Quadtree nw ne sw se) = [nw, ne, sw, se]


prettyPrint qt = show qt

parseChar ch = case ch of
                    '#' -> Alive
                    '-' -> Dead

parseCharStr str = parseChar (head str)

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

data SubQuadChunk = NW | NE | SW | SE

(#) :: Quadtree t -> SubQuadChunk -> Quadtree t
qt # NW = nw qt
qt # NE = ne qt
qt # SW = sw qt
qt # SE = se qt

centeredSubNode (Quadtree (Quadtree {se = newNW}) (Quadtree {sw = newNE})
                         (Quadtree {ne = newSW}) (Quadtree {nw = newSE}))
                        = Quadtree newNW newNE
                                   newSW newSE

centeredHorizontal (Quadtree  _ Quadtree {se = newNW}
                              _ Quadtree {ne = newSW})
                   (Quadtree Quadtree {sw = newNE} _
                             Quadtree {nw = newSE} _)
                  = Quadtree newNW newNE
                             newSW newSE

centeredVertical (Quadtree _ _
                           Quadtree {se = newNW} Quadtree {sw = newNE})
                 (Quadtree Quadtree {ne = newSW} Quadtree {nw = newSE}
                           _ _)
                 = Quadtree newNW newNE
                            newSW newSE

pad qt = Quadtree (Quadtree emptyTree emptyTree emptyTree (nw qt))
                  (Quadtree emptyTree emptyTree (ne qt) emptyTree)
                  (Quadtree emptyTree (sw qt) emptyTree emptyTree)
                  (Quadtree (se qt) emptyTree emptyTree emptyTree)
          where dep = depth qt
                emptyTree = deadTreeFor (dep - 1)

padBy 0 qt = qt
padBy n qt = padBy (n-1) (pad qt)

deadTreeFor :: (Integral i) => i -> Quadtree t
deadTreeFor 0 = Dead
deadTreeFor n = Quadtree (deadTreeFor $ n-1) (deadTreeFor $ n-1) (deadTreeFor $ n-1) (deadTreeFor $ n-1)