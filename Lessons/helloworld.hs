-- main = putStrLn "hello, world"

main = do
    putStrLn "hello there, what's your name?"
    name <- getLine
    putStrLn ("that's a nice name, " ++ name)