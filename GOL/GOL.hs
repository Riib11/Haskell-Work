module GOL
(
) where

import qualified Data.Vector as V


-- example grid

grid = mkGrid
    [[Live,Live,Dead]
    ,[Live,Dead,Live]
    ,[Live,Live,Live]]


-- Cell

data Cell =
    Cell
        { old_state :: State
        , state     :: State }
  | NCell

instance Show Cell where
    show cell = show $ state cell

data State = Live | Dead | Unset

instance Show State where
    show Live = "X"
    show Dead = " "

mkCell :: Cell -> State -> Cell
mkCell c st = Cell
    { old_state = state c
    , state     = st}

mkNewCell :: State -> Cell
mkNewCell st = Cell
    { old_state = Unset
    , state     = st }

isLive :: Cell -> Bool
isLive c = case state c of
    Live -> True
    Dead -> False


-- Grid

data Grid = Grid [[Cell]]

instance Show Grid where
    show (Grid []    ) = show " "
    show (Grid (x:[])) = show x
    show (Grid (x:xs)) = (show x) ++ "\n" ++ (show $ Grid xs)

mkGrid :: [[State]] -> Grid
mkGrid col = Grid $ mkCol col
    where mkCol []     = []
          mkCol (x:xs) = mkRow x : mkCol xs
          mkRow []      = []
          mkRow (y:ys)  = mkNewCell y : mkRow ys


-- Logic 

updateGrid :: Grid -> Grid
updateGrid (Grid col) = Grid $ updateCol col
    where updateCol []          = [] : []
          updateCol (a:    [])  = updateRow [] a [] : []
          updateCol (a:b:  [])  = updateRow a b [] : []
          updateCol (a:b:c:xs)  = updateRow a b c : updateCol xs
          -- base case
          updateRow [] [] [] = []
          
          -- only left
          updateRow ls ms [] = updateRow ls ms
            (rowNCell $ length ms)
          
          -- only right
          updateRow [] ms rs = updateRow rs ms
            (rowNCell $ length ms)
          
          -- left and right
          updateRow (l1:l2:l3:ls) (m1:m2:m3:ms) (r1:r2:r3:rs) =
            updateCell m2 [l1,l2,l3, m1,m3, r1,r2,r3]
            : updateRow (l2:l3:ls) (m2:m3:ms) (r2:r3:rs)

          updateRow (l1:l2:ls) (m1:m2:ms) (r1:r2:rs) =
            updateCell m2 [l1,l2, m1, r1,r2]
            : updateRow (l2:ls) (m2:ms) (r2:rs)
          
          updateRow (l1:ls) (m1:ms) (r1:rs) =
            updateCell m1 [l1,r1]
            : updateRow ls ms rs

rowNCell :: Int -> [Cell]
rowNCell 0 = []
rowNCell x = NCell : rowNCell (x-1)

updateCell :: Cell -> [Cell] -> Cell
updateCell c cs =
    applyNexts c $ countLive cs
    where countLive = length . (filter isLive)

applyNexts :: Cell -> Int -> Cell
applyNexts c x =
    mkCell c (case x of
        1 -> Dead
        2 -> state c    -- same
        3 -> Live       -- birth
        4 -> Dead
        5 -> Dead
        6 -> Dead
        7 -> Dead
        8 -> Dead)