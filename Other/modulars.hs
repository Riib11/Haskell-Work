data Modular = Modular Int Int deriving(Show)
instance Num Modular where
    Modular p x + Modular q y | p==q = Modular p $ mod (x+y) p
    Modular p x * Modular q y | p==q = Modular p $ mod (x*y) p
    abs (Modular p x) = Modular p x
    signum (Modular p x) = Modular p $ signum x
    negate (Modular p x) = Modular p $ (p-x)
    fromInteger _ = error "Modular.fromInteger: not well defined :("