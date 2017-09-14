module Proof where

-- define types
data Prop = Prop (Maybe Bool) deriving(Show)
conj :: Prop -> Prop -> Prop
conj (Prop (Just a)) (Prop (Just b)) = Prop (Just (a && b))
conj _ _ = Prop (Just False)

data Proof = Proof Prop
is_true :: Proof -> Bool
is_true (Proof (Prop (Just a))) = a
is_true _ = False
instance Show Proof where
    show (Proof (Prop (Just True))) = "this is a valid proof."
    show (Proof (Prop (Just False))) = "this is a valid counter-proof."
    show _ = "this is not a valid proof."