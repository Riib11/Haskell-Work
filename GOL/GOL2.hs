module GOL2
( Grid
, Cell(Live,Dead)
, history
, updateGrid
, grid
, size
, indecies
, indecies_flat
, getCell
) where

-- initial conditions

size = 17

grid =
    [[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Live,Live,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Live,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]
    ,[Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead,Dead]]

printGrid :: Grid -> IO ()
printGrid = mapM_ print

printHistory :: [Grid] -> IO ()
printHistory = mapM_ printGrid

printSteps :: Int -> IO ()
printSteps x = printHistory $ take x history

indecies =
    [[(y,x) | x <- r] | y <- r]
    where r = [1..size]

indecies_flat = 
    [(x,y) | x <- r, y <- r]
    where r = [1..size]

localIndecies :: (Int,Int) -> [(Int,Int)]
localIndecies ij = map (shift ij) local_indecies
    where shift (x,y) (u,v) = (u+x,v+y)

local_indecies = remove (0,0) inds
    where inds = [(x,y) | x <- r, y <- r]
          r = [-1..1]

remove :: Eq a => a -> [a] -> [a]
remove y [] = []
remove y (x:xs) = if x == y
    then remove y xs
    else x : remove y xs


-- Logic

stepsGrid :: Grid -> Int -> [Grid]
stepsGrid grid 0 = []
stepsGrid grid x = grid : stepsGrid new_grid (x-1)
    where new_grid = updateGrid grid

history =
    let nextGrids g = ng : nextGrids ng
            where ng = updateGrid g
    in grid : nextGrids grid

updateGrid :: Grid -> Grid
updateGrid grid = map (map (updateCell grid)) indecies

updateCell :: Grid -> (Int,Int) -> Cell
updateCell grid (x,y) = newCell count_live
    where
        count_live = countLive $ map (getCell grid) inds
        inds = localIndecies (x,y)
        cell = getCell grid (x,y)
        newCell n = case n of
            0 -> Dead
            1 -> Dead
            2 -> cell
            3 -> Live
            4 -> Dead
            5 -> Dead
            6 -> Dead
            7 -> Dead
            8 -> Dead

countLive :: [Cell] -> Int
countLive []     = 0
countLive (c:cs) =
    let next = countLive cs
    in case c of
        Live -> next + 1
        Dead -> next

-- Grid of Cells

data Cell = Live | Dead
instance Show Cell where
    show Live = "X"
    show Dead = " "

type Grid = [[Cell]]

-- Access Cells

getCell :: Grid -> (Int,Int) -> Cell
getCell grid (i,j) = getItem (getItem grid i) j

-- mod `size`
getItem :: [a] -> Int -> a
getItem (x:xs) 0 = x
getItem (x:xs) i = if i < 0         -- lower bound
    then getItem (x:xs) (i+size)
    else if i > (size-1)            -- upper bound
        then getItem (x:xs) (i-size)    
        else getItem xs (i-1)

-- Utilities

flatten :: [[a]] -> [a]
flatten [[]] = []
flatten ([]:ys) = flatten ys
flatten ((x:xs):ys) = x : flatten (xs:ys)