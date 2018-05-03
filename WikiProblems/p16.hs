dropEvery :: [a] -> Int -> [a]
dropEvery xs n =
    let
        deh :: [a] -> Int -> [a]
        deh []     _ = []
        deh (x:xs) 1 = deh xs n
        deh (x:xs) i = x : deh xs (i-1)
    in
        deh xs n