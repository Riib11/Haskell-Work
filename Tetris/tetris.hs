module Tetris
(
) where

-- |-----------------------
-- | STATE
-- |-----------------------

data State = State
    { grid      :: Grid
    , pieces    :: [Piece]
    , location  :: Location
    , rotation  :: Rotation }

---- setters
set_grid     (State _ ps loc rot) g   = State g ps loc rot
set_pieces   (State g _  loc rot) ps  = State g ps loc rot
set_location (State g ps _   rot) loc = State g ps loc rot
set_rotation (State g ps loc _  ) rot = State g ps loc rot
----

state_init :: State
state_init = State grid_init pieces_init
    where
        pieces_init = []
        grid_init = take grid_height $ repeat
            (take grid_width $ repeat Open)

-- |-----------------------
-- | GRID
-- |-----------------------

-- each space is either occupied or not
type Grid = [[GridEntry]]

grid_width = 5
grid_height = 20

data GridEntry
    = Closed -- occupied by inactive block
    | Open   -- not occupied by any block
    | Active -- occupied by active block
    deriving (Show)

get_at :: [a] -> Int -> Maybe a
get_at ls i = case ls of
    [] -> Nothing
    (x:xs) -> case i of
        0 -> Just x
        i -> get_at ls (i-1)

get_gridentry :: Grid -> Location -> Maybe GridEntry
get_gridentry grid (x,y) =
    case get_at grid y of
        Nothing -> Nothing
        Just row -> case get_at row x of
            Nothing -> Nothing
            Just e -> Just e

strings_to_grid :: [String] -> Grid
strings_to_grid s = map helper s
    where
        helper :: String -> [GridEntry]
        helper s = case s of
            [] -> []
            (c:cs) -> if c == 'X'
                then Closed : helper cs
                else Open   : helper cs

grid_to_string :: Grid -> String
grid_to_string g = foldl (++) "" (map helper g)
    where
        helper :: [GridEntry] -> String
        helper row = case row of
            [] -> "\n"
            (e:es) -> case e of
                Closed -> 'X' : helper es
                _      -> ' ' : helper es

put_grid :: Grid -> IO ()
put_grid g = putStr $ grid_to_string g

-- |-----------------------
-- | PIECE
-- |-----------------------

data Piece
    = B | T | Sl | Sr | Ll | Lr | I
    deriving (Show)

-- TODO
piece_shape :: Piece -> Rotation -> [[GridEntry]]
piece_shape p rot = strings_to_grid $
    case p of

        B -> ["XX"
             ,"XX"]
        
        T -> case rot of
            R0 -> ["XXX"
                  ," X "]
            
            R1 -> [" X"
                  ,"XX"
                  ," X"]
        
            R2 -> [" X "
                  ,"XXX"]

            R3 -> ["X "
                  ,"XX"
                  ,"X "]

        _ -> [[]] -- TODO: other pieces

check_piece_overlap :: Grid -> Piece -> Location -> Rotation -> Bool
check_piece_overlap grid p loc rot = helper shape loc
    where
        helper :: [[GridEntry]] -> Location -> Bool
        helper shape_cols (x,y) =
            case shape_cols of
                [] -> True
                (c:cs) -> case check_row_overlap c (x,y) of
                    False -> False
                    True  -> helper cs (x + 1, y)

        check_row_overlap :: [GridEntry] -> Location -> Bool
        check_row_overlap shape_row (i,j) =
            case shape_row of
                [] -> True
                (e:es) -> case get_gridentry grid (i,j) of
                    Just Open -> check_row_overlap es (i, j + 1)
                    _ -> False

        shape = piece_shape p rot

-- |-----------------------
-- | DIRECTION + ROTATION
-- |-----------------------

data Direction
    = Left | Right
    deriving (Show)

data Rotation
    = R0 | R1 | R2 | R3
    deriving (Show)

-- |-----------------------
-- | ACTION
-- |-----------------------

data Action
    = Turn Direction
    | Move Direction
    | Gravity
    deriving (Show)

-- |-----------------------
-- | LOCATION
-- |-----------------------

type Location = (Int,Int) -- x,y

loc_init = (div grid_width 2, 0)

-- |-----------------------
-- | GAME LOGIC
-- |-----------------------

-- TODO
place_piece :: State -> Piece -> Maybe State
place_piece state p =

-- TODO
remove_active_piece :: State -> State
remove_active_piece state = state

-- TODO
rotate_active_piece :: State -> Direction -> Maybe State
rotate_active_piece state dir = Nothing

-- TODO
move_active_piece :: State -> Direction -> Maybe State
move_active_piece state dir = Nothing

-- TODO
deactivate_active_piece :: State -> State
deactivate_active_piece state = state 

-- TODO
update :: State -> Action -> Maybe State
update _ _ = Nothing