module Quadtree where

data Quadtree t
  = Cell t
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
prettyPrint (Quadtree ul ur ll lr) =
  foldr
    (++)
    []
    [ unlines $ zipWith (++) (lines $ prettyPrint ul) (lines $ prettyPrint ur)
    , unlines $ zipWith (++) (lines $ prettyPrint ll) (lines $ prettyPrint lr)
    ]

