-- needs more implementation of course
-- data MyMaybe a = MyNothing | MyJust a deriving (Show)
-- instance Functor MyMaybe where
--     fmap f (MyJust x) = MyJust (f x)
--     fmap f MyNothing = MyNothing

-- note that wrote ..Functor Maybe where.. rather than
-- Functor ..(Maybe m) where..

-- fmap (++ "hello") ["a","b","c"]

-- class
class Tofu t where
    tofu :: j a -> t a j
-- type
data Toda a b = Toda { field :: b a } deriving (Show)
-- make type an instance of class
instance Tofu Toda where
    tofu x = Toda x

tofuEx = tofu (Just 1) :: Toda Int Maybe

-- type
data Barry t k p = Barry { yabba :: p, dabba :: t k }
-- make type an instance of class
instance Functor (Barry a b) where
    fmap f (Barry { yabba = x, dabba = y }) = Barry { yabba = f x, dabba = y }


-- LinkedList
data LinkedList a = LinkedNode a (LinkedList a) | EmptyNode deriving(Show)
instance Functor LinkedList where
    fmap f (LinkedNode n ns) = LinkedNode (f n) (fmap f ns)
    fmap f EmptyNode = EmptyNode

arrayToLinkedList :: [a] -> (LinkedList a)
arrayToLinkedList (x:xs)    = LinkedNode x (arrayToLinkedList xs)
arrayToLinkedList []        = EmptyNode
