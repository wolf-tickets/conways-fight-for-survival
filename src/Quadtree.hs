module Quadtree where

data Quadtree t
  = Cell t
<<<<<<< HEAD
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
=======
  | Quadtree { ul :: Quadtree t
             , ur :: Quadtree t
             , ll :: Quadtree t
             , lr :: Quadtree t }
  deriving (Show)

-- depth qt => returns the depth of the Quadtree; depth of a cell is 0
depth :: Quadtree t -> Int
depth (Cell v) = 0
depth (Quadtree ul ur ll lr) = (1 + maximum (map depth [(ul), (ur), (ll), (lr)]))

-- produce Quadtree t from list of four Quadtree t
qtFromList :: [Quadtree t] -> Quadtree t
qtFromList [ul, ur, ll, lr] = Quadtree ul ur ll lr

-- Get list of subtrees from Quadtree
qtToList :: (Quadtree t) -> [Quadtree t]
qtToList (Quadtree ul ur ll lr) = [ul, ur, ll, lr]


--prettyPrint -- Produces a nice string representation
--                of a given Quadtree, works for Quadtree Char
--                with subtrees of equal depth.
prettyPrint :: Quadtree Char -> String
prettyPrint (Cell val) = [val]
<<<<<<< HEAD
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

