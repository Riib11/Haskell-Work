butLast :: [a] -> a
butLast (x:y:[]) = x
butLast (x:xs)   = butLast xs