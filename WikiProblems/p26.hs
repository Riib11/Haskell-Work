{-# LANGUAGE ScopedTypeVariables #-}

nthtail :: Int -> [a] -> [a]
nthtail 0 xs = xs
nthtail _ [] = []
nthtail n (x:xs) = nthtail (n-1) xs

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs =
    if null t
        then []
        else go n xs t
            where
                -- n should always be >= 1 here
                t = nthtail (n-1) xs
                go 0 _ _  =  [[]]
                go _ _ [] = []
                go n xs@(x:xt) t = withx ++ withoutx
                    where
                        withx = map (x:) $ go (n-1) xt t
                        withoutx = go n xt (tail t)