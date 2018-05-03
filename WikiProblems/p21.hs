insertAt :: a -> [a] -> Int -> [a]
insertAt v []     _ = [v]
insertAt v (x:xs) 0 = v:x:xs
insertAt v (x:xs) i = x : insertAt v xs (i-1)