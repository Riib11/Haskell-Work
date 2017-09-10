main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        sys    = map (\[a,b,c] -> Section a b c) threes
        path   = shortestPath sys
        string = concat $ map (show.fst) path
        price  = sum $ map snd path
    putStrLn $ "Shortest path: " ++ string
    putStrLn $ "Price: " ++ show price


-- System
type System = [Section]
data Section = Section { getA::Int, getB::Int, getC::Int } deriving(Show)

heathrowToLondon :: System
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]
-- heathrowToLondon = 
--     [Section 50 10 30
--     ,Section 5  90 20
--     ,Section 40 2  25
--     ,Section 10 8  0]

-- Path
type Path = [(Label,Int)]
data Label = A | B | C deriving(Show)

-- function for shortest path
shortestPath :: System -> Path
shortestPath sys =
    let (pathA,pathB) = foldl sectionStep ([],[]) sys
    in if sum (map snd pathA) <= sum (map snd pathB)
        then reverse pathA
        else reverse pathB

sectionStep :: (Path,Path) -> Section -> (Path,Path)
sectionStep (pathA,pathB) (Section a b c) =
    let priceA      = sum $ map snd pathA
        priceB      = sum $ map snd pathB
        straightToA = priceA + a
        crossToA    = priceB + b + c
        straightToB = priceB + b
        crossToB    = priceA + a + c
        newToA      = if straightToA <= crossToA
                        then (A,a):pathA
                        else (C,c):(B,b):pathB
        newToB      = if straightToB <= crossToB
                        then (B,b):pathB
                        else (C,c):(A,a):pathA
    in (newToA,newToB)

-- turn array into x arrays of n
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf n [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)