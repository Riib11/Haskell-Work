isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []       = True
isPalindrome (x:[])   = True
isPalindrome (x:y:[]) = True
isPalindrome (x:xs)   = ends && next
    where ends = (x == last xs)
          next = isPalindrome (take (length xs - 1) xs)