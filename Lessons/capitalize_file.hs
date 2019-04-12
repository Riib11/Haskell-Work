import System.Environment
import qualified Data.Char as Char

main = do
    [file_in, file_out] <- getArgs
    text <- readFile file_in
    writeFile file_out $ map Char.toUpper text