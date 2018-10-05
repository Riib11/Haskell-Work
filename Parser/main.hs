import System.IO  
import Control.Monad
import Parser

main = do  
        input <- readFile "input/test"
        putStr $ concat
            [ seperator, "\n"
            , (show (parse input))
            , "\n\n", seperator ]


seperator = (take 50 . concat $ repeat "=") ++ "\n"