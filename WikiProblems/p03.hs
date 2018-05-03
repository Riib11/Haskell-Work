elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) i = elementAt xs (i-1)