module UtilList
( pop
) where

pop :: [a] -> Maybe (a,[a])
pop [] = Nothing
pop (x:xs) = Just (x,xs)