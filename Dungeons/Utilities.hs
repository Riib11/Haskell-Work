module Utilities
( removeFromList
, setUnion
) where

removeFromList :: Eq a => a -> [a] -> [a]
removeFromList y []     = []
removeFromList y (x:xs) =
    let next = removeFromList y xs
    in if y == x
        then next
        else x : next

setUnion :: (a -> a -> Bool) -> [a] -> [a] -> [a]
setUnion check []     ys     = ys
setUnion check (x:xs) (y:ys) =
    helper x (y:ys) ++ setUnion check xs (y:ys)
    where helper z []     = [z]
          helper z (w:ws) = if check z w
            then [] else helper z ws