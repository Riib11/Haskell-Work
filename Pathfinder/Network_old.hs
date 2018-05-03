import Grid

-- Node

data Node = Node Edge Edge Edge Edge
    deriving (Eq,Show)

newNode :: Node
newNode = Node Stub Stub Stub Stub

getEdge :: Direction -> Node -> Edge
getEdge dir (Node u d l r) = case dir of
    DirUp    -> u
    DirDown  -> d
    DirLeft  -> l
    DirRight -> r

setEdge :: Direction -> Edge -> Node -> Node
setEdge dir e (Node u d l r) = case dir of
    DirUp    -> Node e d l r
    DirDown  -> Node u e l r
    DirLeft  -> Node u d e r
    DirRight -> Node u d l e

incEdge :: Direction -> Node -> Node
incEdge dir node = setEdge dir new_edge node
    where new_edge = incLength $ getEdge dir node

-- Direction

data Direction = DirUp | DirDown | DirLeft | DirRight
    deriving (Eq,Show,Enum)

-- Edge

data Edge
    = Edge
        { target :: Node
        , length :: Int }
    | Stub
  deriving (Eq,Show)

incLength :: Edge -> Edge
incLength (Edge t l) = Edge t (l+1)
incLength Stub = Edge newNode 1

addLocations :: (Int,Int) -> (Int,Int) -> (Int,Int)
addLocations (a,b) (c,d) = (a+c,b+d)

-- make Network

makeNetwork :: Grid -> [[Node]]
makeNetwork grid = makeNextNetworkRow first second
    where
        first = makeFirstNetworkRow grid
        next = drop 1 $ gridmatrix grid

makeFirstNetworkRow :: Grid -> [Node]
makeFirstNetworkRow grid = helper first Nothing
    where
        first = take 1 $ gridmatrix grid
        helper :: [GridTile] -> Maybe Node -> [Node]
        helper (c:cs) curr = case curr of
            -- continuing an edge
            Just node -> case c of
                -- end of current edge
                GridTileWall -> node : helper cs Nothing
                -- continue current edge
                _ -> helper cs (Just $ incEdge DirRight node)
            -- no current edge
            Nothing -> case c of
                -- consecutive wall
                GridTileWall -> helper cs Nothing
                -- new node opening
                _ -> helper cs (Just newNode)

makeNextNetworkRow :: [Node] -> [[GridTile]] -> [Node]
makeNextNetworkRow net_prevs (r:rs) = net_row : makeNextNetworkRow net_row 

-- makeNetworkRow :: [Node] -> [GridTile] -> [Node]
-- makeNetworkRow prevs row = helper prevs

-- gridToNetwork :: Grid -> Node
-- gridToNetwork grid = Node 
--     let
--         start_loc = gridStartLocation grid
--     in
--         Node (start DirUp) (start DirDown) (start DirLeft) (start DirRight)