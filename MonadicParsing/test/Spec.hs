import System.IO
import Parser
import SimpleLang02 (program)

input = "1 + 1 * 100 - 5"

main :: IO ()
main = do
  putStrLn ""
  putStrLn $ take 40 $ repeat '='
  
  let parsed = runParser program input
  let (output, rest) = head $ parsed
  
  putStr     input
  putStr     " => "
  putStrLn $ show parsed
  
  putStrLn $ take 40 $ repeat '='

