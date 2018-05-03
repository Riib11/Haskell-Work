{-# LANGUAGE ScopedTypeVariables #-}

import System.Random

randomSelect :: forall a. (Eq a) => StdGen -> [a] -> Int -> [a]
randomSelect gen xs n =
    let
        rsh :: StdGen -> [a] -> Int -> [a]
        rs  _   [] _ = []
        rsh _   ys 0 = []
        rsh gen ys i =
            let
                (index, newGen) = randomR (0,i) gen
            in
                (elemAt ys index) : (rsh newGen (removeAt ys index) (i-1))
    in
        rsh gen xs n

-- from p20
removeAt :: (Eq a) => [a] -> Int -> [a]
removeAt []     _ = []
removeAt (x:xs) 0 = xs
removeAt (x:xs) i = x : removeAt xs (i-1)

-- util
elemAt :: [a] -> Int -> a
elemAt (x:xs) 0 = x
elemAt (x:xs) i = elemAt xs (i-1)