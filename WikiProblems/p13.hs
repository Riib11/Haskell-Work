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

data CodeSymbol a = Multiple Int a | Single a deriving(Show)

encodeDirect :: (Eq a) => [a] -> [CodeSymbol a]
encodeDirect xs =
    let
        packed = pack xs
        edh ((x:[]):ys) = (Single x) : (edh ys)
        edh ((x:xs):ys) = (Multiple (length (x:xs)) x) : (edh ys)
        edh ([]:ys)     = edh ys
        edh []          = []
    in
        edh packed