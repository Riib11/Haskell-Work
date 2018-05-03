slice :: [a] -> Int -> Int -> [a]
slice _      _ 0 = []
slice (x:xs) 1 e = x : slice xs 1 (e-1)
slice (x:xs) s e = slice xs (s-1) (e-1)