import Data.List

solveRPN :: (Num a, Read a) => String -> a
solveRPN  = head . foldl f [] . words
    where   f (x:y:ys) "*" = (y*x) : ys
            f (x:y:ys) "+" = (y+x) : ys
            f (x:y:ys) "-" = (y-x) : ys
            f xs numString = read numString : xs