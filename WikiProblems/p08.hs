compress :: (Eq a) => [a] -> [a]
compress (x:y:xs) =
    if x == y
        then (compress (x:xs))
        else x : (compress (y:xs))
compress xs = xs