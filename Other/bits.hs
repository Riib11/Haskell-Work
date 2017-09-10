data Bit = O | X deriving(Show,Eq,Ord)
-- some operators
instance BooleanOperational Bit where
    band x y | (x,y)==(X,X) = X | otherwise = O
    bor x y | x==X = X | y==X = X | otherwise = O
    bxor x y | (x,y)==(X,O) = X | (x,y)==(O,X) = X | otherwise = O

class BooleanOperational a where
    band :: a -> a -> a
    bor  :: a -> a -> a
    bxor :: a -> a -> a