-- Type Classes

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
-- lol

data TrafficLight = Red | Yellow | Green
-- 'instance' instanciates a class
instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False -- otherwise

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- instance Eq (Maybe m) where
--     Just x == Just y = x == y
--     Nothing == Nothing = True
--     _ == _ == False

-- can't have the above, because doesn't have class constant
-- we want all types of the form Maybe m to be part of the Eq typeclass,
-- but only those types where the m is also part of Eq
-- you need:
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ == False