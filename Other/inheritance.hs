-- Entity: top parent
class Entity a where
    hasProperty :: a -> String -> Bool

class (Entity a) => Existent a where
    hasProperty :: a -> String -> Bool
    hasProperty a "exists" = True
class (Entity a) => Noneexistent a where
    hasProperty a "exists" = False

-- Indv: an individual person
-- data Indv 