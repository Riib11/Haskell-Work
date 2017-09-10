{-
    goal: prove 

    Theorem both_and : (forall A B : Prop, A -> B -> A /\ B).
    Proof.
      intros A B.
      intros proof_A proof_B.
      refine (conj _ _).
        exact proof_A.
        exact proof_B.
    Qed.
-}

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

-- intros
a :: Prop
a = Prop (Just True)
b :: Prop
b = Prop (Just True)

-- conj
a_and_b :: Prop
a_and_b = conj a b

-- exact
proof_a_and_b :: Proof
proof_a_and_b = Proof a_and_b