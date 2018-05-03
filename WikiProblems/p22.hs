range :: Int -> Int -> [Int]
range s e =
    let
        rh :: Int -> Int -> [Int]
        rh i 0 = [i]
        rh i j = i : rh (i+1) (j-1)
    in
        rh s (e-s)