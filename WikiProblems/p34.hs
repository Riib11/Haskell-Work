-- Euler's totient function
-- the number of positive integers
-- coprime to m and less than m
totient_phi :: Int -> Int
totient_phi m =
    let
        helper :: Int -> Int
        helper i =
            if i >= m then 0
            else
                if m `coprime` i then 1 + helper (i + 1)
                else helper (i + 1)
    in
        helper 1

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1