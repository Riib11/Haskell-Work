newtype DiffList a = { getDiffList::[a]->[a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)