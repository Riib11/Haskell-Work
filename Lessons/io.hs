-- main = do
--     c <- getChar
--     if c /= ' '
--         then do
--             putChar c
--             main
--         else return ()

-- OR

import Control.Monad

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main

-- seq = do
--     rs <- Sequence [getLine, getLine, getLine]
--     print rs

-- other control structures

foreverloop = forever $ do
    c <- getChar
    putChar c

forloop = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors

-- INPUT!

-- gettinginput = interact shortLinesOnly  
  
-- shortLinesOnly :: String -> String  
-- shortLinesOnly input =   
--     let allLines = lines input  
--         shortLines = filter (\line -> length line < 10) allLines  
--         result = unlines shortLines  
--     in  result

-- OR

gettinginput = interact $ unlines . filter ((<10) . length) . lines

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines  
    where   isPalindrome xs = xs == reverse xs

checkpalindromes = interact respondPalindromes