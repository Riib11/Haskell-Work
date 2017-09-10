-- all possible right triangles of integer side lengths in range
triangles = [ (a,b,c) | a <- [1..1000], b <- [1..1000], c <- [1..1000], a^2 + b^2 == c^2]