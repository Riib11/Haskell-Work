pack :: (Eq a) => [a] -> [[a]]
pack x =
    let
        ph :: (Eq a) => [a] -> [a] -> [[a]]
        ph (x:xs) (y:ys) =
            if x == y
                then ph xs (x:y:ys)
                else (y:ys) : (ph xs [x])
        ph (x:xs) [] = ph xs [x]
        ph [] ys = [ys]
    in
        ph x []