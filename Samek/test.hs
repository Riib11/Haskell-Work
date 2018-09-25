import System.Console.ANSI

main = do
    setSGR [SetColor Foreground Vivid Red]
    setSGR [SetColor Background Vivid Blue]
    putStrLn "hello world"
    setSGR [Reset]
    putStrLn "default colors"