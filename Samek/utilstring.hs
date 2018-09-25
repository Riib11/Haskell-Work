module UtilString
( split
) where

split :: Char -> String -> [String]
split c str = helper str
    where
        helper :: String -> [String]
        helper s = case split_next [] s of
            (ps, []) -> [ps]
            (ps, ns) -> [ps] ++ (helper ns)

        split_next :: String -> String -> (String, String)
        split_next prevs nexts = case nexts of
            []   -> (prevs, nexts)
            n:ns -> if n == c
                then (prevs, ns)
                else split_next (prevs ++ [n]) ns

index :: String -> Int -> Maybe Char
index [] i = Nothing
index (x:xs) i
    | i < 0     = Nothing
    | i == 0    = Just x
    | otherwise = index xs (i-1)