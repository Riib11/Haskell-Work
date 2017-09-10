max3 :: Int -> Int -> Int -> Int
max3 a b c = max a $ max b c

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft x (l,r)
    | abs (l-r + x) < 4   = Just (l+x,r)
    | otherwise             = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight x (l,r)
    | abs (r-l + x) < 4   = Just (l,r+x)
    | otherwise             = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- example usage of monad operators
a :: Pole
a = (0,0)

ex :: Maybe Pole
ex = landLeft 1 a >>= landRight 2 >>= landLeft (-1) >>= landLeft 4 >>= landRight 2