example1 = [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
example2 = [ (n,ch) | n<-[1,2], ch<-['a','b'] ]

permutations :: [a] -> [b] -> [(a,b)]
permutations l1 l2 = l1 >>= \x -> l2 >>= \y -> return (x,y)