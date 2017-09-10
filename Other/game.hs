action :: IO String
action = do
    a <- getLine
    b <- getLine
    putStrLn $ a ++ " said " ++ b