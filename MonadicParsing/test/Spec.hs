import System.IO
import Parser
import TreeLang

input = "function a => apply a of b"
-- input = "apply a of b"
-- input = "let a := b in a"
-- input = "a"


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
