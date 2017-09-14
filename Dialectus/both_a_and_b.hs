import Proof

{-
    goal: prove the following

    Theorem both_and : (forall A B : Prop, A -> B -> A /\ B).
    Proof.
      intros A B.
      intros proof_A proof_B.
      refine (conj _ _).
        exact proof_A.
        exact proof_B.
    Qed.
-}

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