-- this function allows for maybe-chaining
-- without staircasing

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just x  >>? f = f x

-- example
x :: Maybe Int
x = Just 1
f :: Int -> Maybe Bool
f x | x == 0 = False | otherwise = True

main = do
  y <- 1
