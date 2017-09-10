import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 1000 $ randomRs ('a','z') gen