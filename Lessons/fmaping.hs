data CMaybe a = CJust Int a | CNothing deriving(Show)
instance Functor CMaybe where
    fmap f (CJust i a) = CJust (i+1) (f a)
    fmap f CNothing = CNothing