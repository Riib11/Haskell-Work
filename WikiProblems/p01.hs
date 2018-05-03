myLast :: (Num a) => [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs