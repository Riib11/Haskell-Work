isPrime :: Int -> Bool
isPrime x =
    let
        helper :: Int -> Bool
        helper i =
            if i > sqrt_int x then True else
            if i `divides` x then False
            else helper (x + 1)

    in
        if x == 1 then True
        else helper 2

sqrt_int :: Int -> Int
sqrt_int = floor . sqrt . fromIntegral

divides :: Int -> Int -> Bool
divides a b =
    let
        helper :: Int -> Bool
        helper x =
            if x * a == b then True
            else if a * x > b then False
            else helper (x + 1)
    in
        helper 1