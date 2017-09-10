multThree :: Num n => n -> n -> n -> n
multThree a b c = a * b * c

-- explicit
-- compareWithHundred :: (Ord a, Num a) => a -> Ordering
-- compareWithHundred x = compare 100 x

-- implicit
compareWithHundred :: (Ord a, Num a) => a -> Ordering
compareWithHundred = compare 100

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

apply4 :: (a -> a) -> a -> a
apply4 f x = applyTwice f (applyTwice f x)

largestDivisible :: Integral n => n -> n -> n
largestDivisible max target = head (filter p [max,max-1..])
  where p x = (x `mod` target) == 0

isDivisible :: Integral n => n -> n -> Bool
isDivisible b s = (mod s b) == 0

mults = map (*) [0..]

-- lambdas
addThree :: (Num n) => n -> n -> n -> n
addThree = \x -> \y -> \z -> x + y + z

squares = map (\x -> x * x) [0..]

-- folding
sum' :: Num a => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs
-- or you can use "foldl1" and omit the "0" parameter

-- scanning
summing :: Num a => [a] -> [a]
summing xs = scanl (\x y -> x + y) 0 xs