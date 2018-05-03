module Path
( solveGrid
) where

import Grid

data Direction = DirU | DirR | DirD | DirL
    deriving (Eq,Enum,Bounded,Show)

matchDirectionTo :: (a,a,a,a) -> Direction -> a
matchDirectionTo (x,_,_,_) DirU = x
matchDirectionTo (_,x,_,_) DirR = x
matchDirectionTo (_,_,x,_) DirD = x
matchDirectionTo (_,_,_,x) DirL = x

directions = [minBound..maxBound] :: [Direction]

directionVector :: Direction -> Vector
directionVector = matchDirectionTo ((0,-1),(1,0),(0,1),(-1,0))

goDirection :: Direction -> Vector -> Vector
goDirection dir = vectorAdd $ directionVector dir

type Path = (Bool,[Vector])

addPathStep :: Bool -> Vector -> Maybe Path -> Maybe Path
addPathStep b v (Just (_,vs)) = Just (b,v:vs)
addPathStep _ _ Nothing = Nothing

solveGrid :: Grid -> Maybe Path
solveGrid grid = helper start Nothing
    where
        start = gridStartLocation grid
        finish = gridFinishLocation grid
        helper :: Vector -> Maybe Direction -> Maybe Path
        helper p mdir
            = addPathStep finished np
            ( shortestPath
            $ map (helper np) (directionsExcluding mdir) )
            where
                np = case mdir of
                    Just dir -> goDirection dir
                    Nothing  -> p
                finished = finish == np
                directionsExcluding :: Maybe Direction -> [Maybe Direction]
                directionsExcluding (Just dir) = [ Just d | d <- directions, d/=dir ]
                directionsExcluding Nothing    = [ Just d | d < directions ]
                shortestPath :: [Maybe Path] -> Maybe Path
                shortestPath [] = Nothing
                shortestPath [x] = Just x
                shortestPath (x1:x2:xs) = case (x1,x2) of
                    (Nothing, Nothing) -> shortestPath xs
                    (Just x , Nothing) -> shortestPath (Just x:xs)
                    (Nothing, Just x ) -> shortestPath (Just x:xs)
                    (Just x , Just y ) -> shortestPath (Just
                        ( if (length x) < (length y)
                            then Just x
                            else Just y) : xs )