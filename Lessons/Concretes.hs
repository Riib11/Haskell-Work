import Data.Map

type IntMap v = Map Int v
-- or type IntMap = Map Int

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)