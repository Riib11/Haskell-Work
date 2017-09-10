-- "sum (map sqrt [1..130])"" is the same as
amount = sum $ map sqrt [1..130]

-- composition; not music :(
f :: Num n => n -> n
f x = x * x
g :: Num n => n -> n
g x = (x + 10) * 10

oddSquareSum :: Integer
oddSquareSum =
  let oddSquares = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (<10000) oddSquares
  in sum belowLimit