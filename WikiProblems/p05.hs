rev :: [a] -> [a]
rev (x:xs)  = (rev xs) ++ [x]
rev []      = []