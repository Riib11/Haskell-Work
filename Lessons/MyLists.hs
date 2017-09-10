-- makes a list that holds type a
-- Cons is basically adding an element to the beginning of a list

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- data AlsoList a = Empty | Cons { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- pattern matching like (x :-: xs) only works on matching constructors