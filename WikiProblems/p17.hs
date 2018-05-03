{-# LANGUAGE ScopedTypeVariables #-}

split :: forall a. [a] -> Int -> [[a]]
split xs n =
    let
        sh :: [a] -> Int -> [a] -> [a] -> [[a]]
        sh []     _ hs ts = [hs,ts]
        sh (y:ys) 0 hs ts = sh ys 0 hs (ts++[y])
        sh (y:ys) i hs ts = sh ys (i-1) (hs++[y]) ts
    in
        sh xs n [] []