module Grid
( Grid (Grid)
, Vector
, GridTile (GridTileStart, GridTileFinish, GridTileEmpty, GridTileWall, GridTilePath)
, gridsize
, gridmatrix
, gridMap
, gridtileLocation
, gridStartLocation
, gridFinishLocation
, charToGridTile
, gridtileColor
, gridtileChar
, vectorAdd
, gridFlatten
, gridStack
, gridtileAt
, makeGridFromString
, test
) where

-- Grid

data Grid = Grid Int [[GridTile]]

type Vector = (Int,Int)

gridsize :: Grid -> Int
gridsize (Grid size _) = size

gridmatrix :: Grid -> [[GridTile]]
gridmatrix (Grid _ mat) = mat

gridMap :: (GridTile -> GridTile) -> Grid -> Grid
gridMap f (Grid size grid) = Grid size (map (map f) grid)

gridtileLocation :: GridTile -> Grid -> Vector
gridtileLocation t (Grid _ grid) = helper (0,0) grid
    where
        helper (i,j) ([]:rs) = helper (i+1,0) rs
        helper (i,j) ((c:cs):rs) =
            if c == t
                then (i,j)
                else helper (i,j+1) (cs:rs)

gridStartLocation :: Grid -> Vector
gridStartLocation = gridtileLocation GridTileStart

gridFinishLocation :: Grid -> Vector
gridFinishLocation = gridtileLocation GridTileFinish

instance Show Grid where
    show (Grid size grid) = "Grid " ++ s ++ " : " ++ g
        where
            s = show size ++ "x" ++ show size
            g = show grid

-- GridTile

data GridTile = GridTileStart
              | GridTileFinish
              | GridTileEmpty
              | GridTileWall
              | GridTilePath
              deriving (Eq,Enum)

gridtile_chars =  ['S','F','E','W','*']
gridtile_colors = [ 1 , 1 , 2 , 0 , 1 ]

instance Show GridTile where
    show gt = [getAt gridtile_chars (fromEnum gt)]
        where
            getAt (x:xs) 0 = x
            getAt (x:xs) i = getAt xs (i-1)

charToGridTile :: Char -> GridTile
charToGridTile c =
    if elem c gridtile_chars
        then toEnum $ elemIndex c gridtile_chars
        else GridTileEmpty -- default

gridtileColor :: GridTile -> Int
gridtileColor t = elemAt (fromEnum t) gridtile_colors

gridtileChar :: GridTile -> Char
gridtileChar t = elemAt  (fromEnum t) gridtile_chars

elemIndex y (x:xs) = if x == y then 0 else 1 + elemIndex y xs

elemAt i xs =
    helper xs 0
    where helper (x:xs) j = if j == i then x else helper xs (j+1)

vectorAdd :: Vector -> Vector -> Vector
vectorAdd (a,b) (c,d) = (a+b,c+d)

-- makeGrid

gridFlatten :: Grid -> [GridTile]
gridFlatten (Grid _ grid) = helper grid
    where
        helper :: [[GridTile]] -> [GridTile]
        helper [[]] = []
        helper ([]:ys) = helper ys
        helper ((x:xs):ys) = x : helper (xs:ys)


gridStack :: Int -> [GridTile] -> Grid
gridStack size grid = Grid size $ stacker grid
    where
        stacker :: [GridTile] -> [[GridTile]]
        stacker [] = []
        stacker xs = (take size xs) : stacker (drop size xs)

gridtileAt :: Vector -> Grid -> GridTile
gridtileAt (r,c) grid = elemAt c (gridRowAt r grid)

gridRowAt :: Int -> Grid -> [GridTile]
gridRowAt r (Grid _ grid) = elemAt r grid

makeGridFromString :: Int -> String -> Grid
makeGridFromString size str = gridStack size $ map charToGridTile grid
    where grid = strip str " -,\n"

strip :: String -> String -> String
strip str rm = [ c | c <- str, not (elem c rm) ]

-- Test

test = makeGridFromString 3
    "SEW WEW WEF"