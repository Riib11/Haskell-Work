class Animal a where
    eat :: a -> Food -> Energy
    reproduce :: a -> Energy -> a

class (Animal a) => Cat a where
    meow :: a -> Energy -> Sound

-- types
type Food = Int
type Energy = Int
type Sound = String