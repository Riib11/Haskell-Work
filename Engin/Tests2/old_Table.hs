module Table
( Vector (Vector)
, make_vector
, vec_zero
, vec_join
, vec_prepend
, vec_append
, vec_set
, to_array
, index
, find
, Table
, make_table
, table_empty
, get
, set
) where

import Naturals

-- Tests 

keys = make_vector ['A','B','C']
vals = make_vector [1,2,3]
table = Table keys vals

-- Vectors

data Vector a = Vector Nat [a]
    deriving (Eq)

instance Show a => Show (Vector a) where
    show (Vector n xs) = show xs

vec_zero :: Vector a
vec_zero = Vector Zero []

make_vector :: Eq a => [a] -> Vector a
make_vector xs = Vector (nat_length xs) xs

vec_join :: Vector a -> Vector a -> Vector a
vec_join (Vector n xs) (Vector m ys) = Vector (nat_add n m) (xs ++ ys)

vec_prepend :: Vector a -> a -> Vector a
vec_prepend (Vector n xs) x = Vector (Succ n) (x:xs)

vec_append :: Vector a -> a -> Vector a
vec_append (Vector n xs) x = Vector (Succ n) (xs ++ [x])

vec_set :: Vector a -> Nat -> a -> Vector a
vec_set (Vector Zero xs) i v = Vector Zero xs
vec_set (Vector n (x:xs)) Zero v = Vector n (v:xs)
vec_set (Vector (Succ n) (x:xs)) (Succ i) v =
    vec_prepend (vec_set (Vector n xs) i v) x

to_array :: Vector a -> [a]
to_array (Vector n xs) = xs

index :: Vector a -> Nat -> Maybe a
index (Vector n []) m = Nothing
index (Vector n (x:xs)) Zero = Just x
index (Vector (Succ n) (x:xs)) (Succ m) = index (Vector n xs) m

find :: Eq a => Vector a -> a -> Maybe Nat
find (Vector n xs) y = helper xs Zero
    where
        helper [] _ = Nothing
        helper (x:xs) count =
            if x == y
                then Just count
                else helper xs (nat_add one count)

-- Table

data Table a b = Table (Vector a) (Vector b)
    deriving (Eq,Show)

-- instance (Show a, Show b) => Show (Table a b) where
--     show (Table (Vector Zero _) (Vector Zero _)) = "}"
--     show (Table (Vector (Succ n) (x:xs)) (Vector (Succ m) (y:ys))) =
--         (show x) ++ " = " ++ (show y) ++ "," ++
--         show (Table (Vector n xs) (Vector n ys))

make_table :: Vector a -> Vector b -> Table a b
make_table a b = Table a b

table_empty :: Table a b
table_empty = Table va vb
    where
        va = vec_zero :: Vector a
        vb = vec_zero :: Vector b

get :: Eq a => Table a b -> a -> Maybe b
get (Table va vb) x =
    let i = find va x
    in case i of
        Nothing -> Nothing
        Just n  -> case index vb n of
            Nothing -> Nothing
            Just m  -> Just m

set :: Eq a => Table a b -> a -> b -> Table a b
set (Table va vb) k v =
    let i = find va k
    in case i of
        -- add new entry
        Nothing -> Table (vec_append va k) (vec_append vb v)
        -- change old entry
        Just j  -> Table va (vec_set vb j v)

has :: Eq a => Table a b -> a -> Bool
has (Table va vb) k =
    let i = find va k
    in case i of
        Nothing -> False
        Just _  -> True