{-# LANGUAGE ScopedTypeVariables #-}

rotate :: [a] -> Int -> [a]
rotate []     _ = []
rotate xs     0 = xs
rotate (x:[]) _ = [x]
rotate (x:xs) n =
    if n > 0
        then rotate (xs ++ [x]) (n-1)
        else
            let
                heads :: [a] -> [a]
                heads (x:[]) = []
                heads (x:xs) = x : heads xs
            in
                rotate (last xs:x:heads xs) (n+1)