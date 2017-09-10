combinations :: [a] -> [(a,a,a)]
combinations range = [ (x,y,z) | x<-range, y<-range, z<-range ]