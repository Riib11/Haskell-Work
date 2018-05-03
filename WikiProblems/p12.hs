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

-- from p10
encode :: (Eq a) => [a] -> [(Int,a)]
encode xs =
    let
        packed = pack xs
        eh ((x:xs):ys) = (length (x:xs),x) : (eh ys)
        eh ([]:ys)     = eh ys
        eh []          = []
    in
        eh packed

data CodeSymbol a = Multiple Int a | Single a deriving(Show)

-- from p11
encodeModified :: (Eq a) => [a] -> [CodeSymbol a]
encodeModified xs =
    let
        encoded = encode xs
        emh ((x,v):xs) =
            if x == 1
                then (Single v) : emh xs
                else (Multiple x v) : emh xs
        emh [] = []
    in
        emh encoded

decodeModified :: (Eq a) => [CodeSymbol a] -> [a]
decodeModified []                  = []
decodeModified ((Single v):xs)     = v : decodeModified xs
decodeModified ((Multiple 2 v):xs) = v : (decodeModified ((Single v):xs))
decodeModified ((Multiple x v):xs) = v : (decodeModified ((Multiple (x-1) v):xs))