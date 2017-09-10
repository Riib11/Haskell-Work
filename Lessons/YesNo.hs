------------------------
-- Class Declaration
------------------------

class YesNo a where
    yesno :: a -> Bool

--------------------
-- Instances
--------------------

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _ ) = True
    yesno Nothing = False

-- these are from other files

-- instance YesNo (Tree a) where
--     yesno EmptyTree = False
--     yesno _ = True

-- instance YesNo TrafficLight where
--     yesno Red = False
--     yesno _ = True

--------------------
-- Functions
--------------------
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf v yesResult noResult = if yesno v then yesResult else noResult
