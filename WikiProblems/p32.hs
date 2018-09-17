gcd :: Int -> Int -> Int
gcd a b =
    let
        helper :: Int -> Int
        helper i =
            if and [ i `divides` a
                   , i `divides` b ]
                then i
            else helper (i - 1)
    in
        helper $ min a b

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