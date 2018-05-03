module LinkedGraph where

-----------------------
---- Paramaters
-----------------------

-- just store string for now
type Value = String

-----------------------
---- LinkedGraph
-----------------------

data LinkedGraph = Node { value :: Value , next :: LinkedGraph , connection :: LinkedGraph } | EmptyNode

instance Show LinkedGraph where
    show ( Node v EmptyNode EmptyNode ) = show v
    show ( Node v n         EmptyNode ) = show v ++ " -> (), " ++ show n
    show ( Node v EmptyNode c         ) = show v ++ " -> ( " ++ show c ++ " )"
    show ( Node v n         c         ) = show v ++ " -> ( " ++ show c ++ " ), " ++ show n
    show ( EmptyNode                  ) = "()"

-----------------------
---- Implimentations
-----------------------

-- Creating LinkedGraphs

-- a node with just a value
blankNode :: Value -> LinkedGraph
blankNode val = Node val EmptyNode EmptyNode

singleNode :: Value -> LinkedGraph -> LinkedGraph
singleNode val node = Node val EmptyNode node

-- connects two nodes
(~>) :: LinkedGraph -> LinkedGraph -> LinkedGraph
( Node val EmptyNode EmptyNode ) ~> b = Node val EmptyNode b
( Node val EmptyNode conn      ) ~> b = Node val ( (blankNode val ) ~> b ) conn
( Node val next      conn      ) ~> b = Node val (next ~> b) conn

-- Query

data Query = Query { target :: Value , previous :: [Value] } deriving(Show)

arrayContains :: [Value] -> Value -> Bool
arrayContains (v:vs) t
    | v == t = True
    | otherwise = arrayContains vs t
arrayContains [] t = False


maybeOr :: (Maybe a) -> (Maybe a) -> (Maybe a)
maybeOr (Just x) _        = Just x
maybeOr Nothing  (Just x) = Just x
maybeOr Nothing  Nothing  = Nothing


-- Check if there is a connection between two values
getNode :: LinkedGraph -> Query -> Maybe LinkedGraph

-- empty can't contain a value
getNode EmptyNode _ = Nothing

-- conn and next
getNode (Node val1 (Node val2 next2 conn2) (Node val3 next3 conn3)) (Query target prev)
    | val1 == target = Just (Node val1 next conn)  -- found target!
    | (arrayContains prev val1) = Nothing -- already checked this node
    | otherwise = maybeOr (getNode next query1) (getNode conn query2)
        where next = (Node val2 next2 conn2) -- check next
              conn = (Node val3 next3 conn3) -- and connection
              query1 = (Query target prev) 
              query2 = (Query target (val1:prev))

-- only next
getNode (Node val1 (Node val2 next2 conn2) EmptyNode) (Query target prev)
    | val1 == target = Just (Node val1 next conn)  -- found target!
    | (arrayContains prev val1) = Nothing -- already checked this node
    | otherwise = getNode next query -- check next
        where next  = (Node val2 next2 conn2)
              conn  = EmptyNode
              query = (Query target prev)

-- only conn
getNode (Node val1 EmptyNode (Node val2 next2 conn2)) (Query target prev)
    | val1 == target = Just (Node val1 next conn)  -- found target!
    | (arrayContains prev val1) = Nothing -- already checked this node
    | otherwise = getNode conn query -- check connection
        where next  = EmptyNode
              conn  = (Node val2 next2 conn2)
              query = (Query target (val1:prev))

-- neither conn nor next
getNode (Node val1 EmptyNode EmptyNode) (Query target prev)
    | val1 == target = Just (Node val1 EmptyNode EmptyNode) -- found target!
    | otherwise = Nothing -- never found it :(


-- automates Query
getValueNode :: LinkedGraph -> Value -> (Maybe LinkedGraph)
getValueNode lg val = getNode lg (Query val [])

----
isSomething :: (Maybe a) -> Bool
isSomething (Just x) = True
isSomething Nothing  = False

isNodeConnected :: (Maybe LinkedGraph) -> Value -> Bool
isNodeConnected (Just src)       tgt = isSomething $ getValueNode src tgt
isNodeConnected Nothing          _   = False

-- check connection within LinkedGraph
isConnected :: LinkedGraph -> Value -> Value -> Bool
isConnected lg source target = isNodeConnected sourceNode target
    where sourceNode = getValueNode lg source