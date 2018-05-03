module Network
(
) where

import Grid

-- Node

data Node
    = Node (Int,Int) (Node,Node,Node,Node) -- up,down,left,right
    | Stub
    deriving (Eq,Show)

nodeLocation :: Node -> (Int,Int)
nodeLocation (Node p _) = p

nextNode :: Node -> Direction -> Node
nextNode (Node _ nexts) = matchDirectionTo nexts

-- Direction

data Direction = DirUp | DirRight | DirDown | DirLeft | DirNone
    deriving (Eq,Enum,Show)

matchDirectionTo :: (a,a,a,a) -> Direction -> a
matchDirectionTo (x,y,z,w) dir = case dir of
    DirUp    -> x
    DirRight -> y
    DirDown  -> z
    DirLeft  -> w

directionVector :: Direction -> (Int,Int)
directionVector DirNone = (0,0)
directionVector dir = matchDirectionTo
    ((0,-1),(1,0),(0,1),(-1,0))
    dir

goDirection :: Direction -> (Int,Int) -> (Int,Int)
goDirection dir p = vectorAdd p $ directionVector dir

nextDirections :: Direction -> (Bool,Bool,Bool,Bool)
nextDirections DirNone = (True,True,True,True)
nextDirections dir = matchDirectionTo
    ( (True,True,False,True)
    , (True,True,True,False)
    , (False,True,True,True)
    , (True,False,True,True) )
    dir

qud 0 (x,_,_,_) = x
qud 1 (_,x,_,_) = x
qud 2 (_,_,x,_) = x
qud 3 (_,_,_,x) = x

qudMap f (a,b,c,d) = (f a,f b,f c,f d)

vectorAdd :: (Int,Int) -> (Int,Int) -> (Int,Int)
vectorAdd (x,y) (z,w) = (x+z,y+w)

vectorBounded :: Int -> (Int,Int) -> Bool
vectorBounded s (x,y) = x>=0 && x<s && y>=0 && y<s

-- Network

makeNetwork :: Grid -> Node
makeNetwork grid = makeNextNode grid start DirNone
    where start = gridStartLocation grid

makeNextNode :: Grid -> (Int,Int) -> Direction -> Node
makeNextNode grid p dir =
    let
        next_dirs       = nextDirections dir
        np              = goDirection dir p
        next            = makeNextNode grid np
        nextDirection i = if qud i next_dirs
            then next $ (toEnum i :: Direction)
            else Stub
    in if vectorBounded (gridsize grid) np
        then case gridtileAt np grid of
            GridTileWall -> Stub
            _ -> Node np $ qudMap nextDirection (0,1,2,3)
        else Stub

-- test

test_net = makeNetwork test
test_next = nextNode test_net