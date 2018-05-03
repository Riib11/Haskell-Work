module Naturals
( Nat (Zero, Succ)
, one,two,three,four,five,six
, nat_add
, nat_length
, to_nat
, to_int
) where

-- The Natural Numbers

data Nat = Zero | Succ Nat
    deriving (Ord,Eq)

one = Succ Zero
two = Succ one
three = Succ two
four = Succ three
five = Succ four
six = Succ five

instance Show Nat where
    show Zero = "O"
    show (Succ x) = "S" ++ show x

nat_add :: Nat -> Nat -> Nat
nat_add Zero Zero = Zero
nat_add (Succ x) Zero = Succ x
nat_add Zero (Succ y) = Succ y
nat_add (Succ x) (Succ y) = Succ $ nat_add (Succ x) y

nat_length :: [a] -> Nat
nat_length [] = Zero
nat_length (x:xs) = Succ $ nat_length xs

to_nat :: Int -> Maybe Nat
to_nat 0 = Just Zero
to_nat x = if x > 0
    then case to_nat (x-1) of
        Just n -> Just $ nat_add one n
        Nothing -> Nothing
    else Nothing

to_int :: Nat -> Int
to_int Zero = 0
to_int (Succ n) = 1 + to_int n