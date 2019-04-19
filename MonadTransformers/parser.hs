import Control.Monad.Trans.State

-- a parser is just an instance of a StateT
type Parser = StateT String []

-- running a parser is just running a generic state
-- and extracting the output
runParser :: Parser a -> String -> [a]
runParser p s = [ x | (x, "") <- runStateT p s ]

-- an example parser
-- parse_head will only parse if the input is a single character
parse_head :: Parser Char
parse_head = do
  c:cs <- get
  put cs
  return c

