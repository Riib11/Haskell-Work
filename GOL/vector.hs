

data Dim = Succ Dim | Zero

one     = Succ Zero
two     = Succ one
three   = Succ two
four    = Succ three

data Vector n a where
    Nil     :: Vector Zero a
    Cons    :: a -> Vector n a -> Vector (Succ n) a