import System.Environment
import Grid
 
main = do
    [filename] <- getArgs
    gridstring <- readFile filename
    
    -- do stuff
    let new_gridstring = id gridstring
    
    let new_filename = tagSolved filename
    writeFile new_filename new_gridstring

tagSolved :: String -> String
tagSolved [] = []
tagSolved (c:cs) =
    if c == '.'
        then "_solved." ++ cs
        else c : tagSolved cs