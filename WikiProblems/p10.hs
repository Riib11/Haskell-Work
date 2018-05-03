-- from p09
pack :: (Eq a) => [a] -> [[a]]
pack x =
    let
        ph :: (Eq a) => [a] -> [a] -> [[a]]
        ph (x:xs) (y:ys) =
            if x == y
                then ph xs (x:y:ys)
                else (y:ys) : (ph xs [x])
        ph (x:xs) [] = ph xs [x]
        ph []     ys = [ys]
    in
        ph x []

encode :: (Eq a) => [a] -> [(Int,a)]
encode xs =
    let
        packed = pack xs
        eh ((x:xs):ys) = (length (x:xs),x) : (eh ys)
        eh ([]:ys)     = eh ys
        eh []          = []
    in
        eh packed