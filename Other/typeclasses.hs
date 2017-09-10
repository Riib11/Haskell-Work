import Data.Rational

type Q = Q Rational deriving(Eq,Ord,Show,Fractional)

instance Show Q where
    show (Q x)
        | b == 1    = show a
        | otherwise = show a ++ "/" ++ show b
        where   a = numerator x
                b = demonimator x
------------------------
-- Possible Approach:
------------------------
-- data Fp = Fp Integer Integer
-- instance Num Fp where
--     F p x + F q y | p==q = F p $ mod (x+y) p
--     F p x * F q y | p==q = F p $ mod (x*y) p
--     fromInteger _ = error "Fp.fromInteger: not well defined"

------------------------
-- Better Approach
------------------------
type Fp n = Fp Integer

-- fields
type F2 = Fp T2
type F3 = Fp T3
type F5 = Fp T5
type F7 = Fp T7
-- primes
data P2
data P3
data P5
data P7
class IntegerAsType n => Num (Fp n) where
    Fp x + Fp y = Fp $ mod (x+y) p where p = value (undefined :: n)
    Fp x * Fp y = Fp $ mod (x*y) p where p = value (undefined :: n)
    fromInteger m = Fp $ m `mod` p where p = value (undefined :: n)
    -- value :: a -> Integer
instance IntegerAsType P2 where value _ = 2
instance IntegerAsType P3 where value _ = 3
instance IntegerAsType P5 where value _ = 5
instance IntegerAsType P7 where value _ = 7