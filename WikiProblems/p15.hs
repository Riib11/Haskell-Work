repli :: [a] -> Int -> [a]
repli xs n =
    let
        rh :: [a] -> Int -> [a]
        rh []     _ = []
        rh (x:xs) 0 = rh xs n
        rh (x:xs) i = x : rh (x:xs) (i-1)
    in
        rh xs n