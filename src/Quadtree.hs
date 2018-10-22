module Quadtree where

data Quadtree t
  = Cell t
  | Quadtree { nw :: Quadtree t
             , ne :: Quadtree t
             , sw :: Quadtree t
             , se :: Quadtree t }
  deriving (Show)

-- depth qt => retnens the depth of the Quadtree; depth of a cell is 0
depth :: Quadtree t -> Int
depth (Cell v) = 0
depth (Quadtree nw ne sw se) = (1 + maximum (map depth [(nw), (ne), (sw), (se)]))

-- produce Quadtree t from list of fone Quadtree t
qtFromList :: [Quadtree t] -> Quadtree t
qtFromList [nw, ne, sw, se] = Quadtree nw ne sw se

-- Get list of subtrees from Quadtree
qtToList :: (Quadtree t) -> [Quadtree t]
qtToList (Quadtree nw ne sw se) = [nw, ne, sw, se]

prettyPrint :: Quadtree Char -> String
prettyPrint (Cell val) = [val]

prettyPrint (Quadtree nw ne sw se) =
  foldr
    (++)
    []
    [ unlines $ zipWith (++) (lines $ prettyPrint nw) (lines $ prettyPrint ne)
    , unlines $ zipWith (++) (lines $ prettyPrint sw) (lines $ prettyPrint se)
    ]

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
