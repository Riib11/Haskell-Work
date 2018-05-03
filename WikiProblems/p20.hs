removeAt :: (Eq a) => [a] -> Int -> [a]
removeAt []     _ = []
removeAt (x:xs) 0 = xs
removeAt (x:xs) i = x : removeAt xs (i-1)