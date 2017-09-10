mx :: (Ord a) => [a] -> a
mx [] = error "maximum of empty list"
mx [x] = x
mx (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = mx xs

rep :: (Num n, Ord n) => x -> n -> [x]
rep x n
  | n <= 0 = []
  | otherwise = x:(rep x (n-1))

take' :: (Num n, Ord n) => n -> [x] -> [x]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:(take' (n-1) xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

repeater :: a -> [a]
repeater a = a:(repeater a)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smaller = quicksort [ a | a <- xs, a <= x]
      bigger  = quicksort [ a | a <- xs, a > x ]
  in smaller ++ [x] ++ bigger
