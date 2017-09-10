import Data.List as List
import Data.Map as Map
import Data.Set as Set

periods = intersperse '.' "hello there"
zeros = intercalate [0] [[1..3],[1..3],[1..3]]

m = [[1,0,1],[1,1,1],[1,0,1]]
m' = transpose m

alphabet :: Integer -> [Char]
alphabet 1 = ['a'..'z']
alphabet x = concat [['a'..'z'],alphabet (x-1)]

-- iterate (infinite)
powers :: Num n => n -> [n]
powers x = iterate (*x) 1

alpha n = take n $ iterate (\x -> concat [['a'..'z'],x]) ['a'..'z']

-- `union`, `intersect`, // (list difference)

-- dictionary looks like this
people =
    [("henry",12345)
    ,("kayleigh",6789)
    ]

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . Prelude.filter (\(k,v) -> key == k) $ xs

-- sets
-- Set. .. has a lot of functions to play with that are pretty self-explanatory

letters1 = Set.fromList "hello there! this is a sentence with some letters."
letters2 = Set.fromList "hello again, this is yet another sentence with some letters."