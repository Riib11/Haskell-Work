myFlatten :: [[a]] -> [a]
myFlatten ([]:[]) = []
myFlatten ((x:xs):[]) = xs
myFlatten ((x:xs):xss) = xs : (myFlatten xss)