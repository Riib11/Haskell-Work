{-# LANGUAGE ScopedTypeVariables #-}

import System.Random
import Debug.Trace

randPermutation :: forall a. (Show a) => StdGen -> [a] -> [a]
randPermutation gen [] = []
randPermutation gen xs =
    let
        l = length xs
        pi = randomRange gen l (l+1)

        gen_perm :: Int -> [a]
        gen_perm 1 = []
        gen_perm i = (elemAt xs (elemAt pi (i-1))) : (gen_perm (i-1))
    in
        trace gen_perm l


-- from p24
randomRange :: StdGen -> Int -> Int -> [Int]
randomRange gen n x = randomSelect gen (range 1 x) n

-- from p20
removeAt :: (Eq a) => [a] -> Int -> [a]
removeAt []     _ = []
removeAt (x:xs) 0 = xs
removeAt (x:xs) i = x : removeAt xs (i-1)

-- util
elemAt :: [a] -> Int -> a
elemAt (x:xs) 0 = x
elemAt (x:xs) i = elemAt xs (i-1)

-- from p23
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

-- from p22
range :: Int -> Int -> [Int]
range s e =
    let
        rh :: Int -> Int -> [Int]
        rh i 0 = [i]
        rh i j = i : rh (i+1) (j-1)
    in
        rh s (e-s)