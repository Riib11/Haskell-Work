-- pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = "Nice!"
lucky x = "You suck..."

-- recursion
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

sum' :: (Num n) => [n] -> n
sum' [] = 0
sum' (x:xs) = x + sum' xs

