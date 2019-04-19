import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Monoid

type Parser = WriterT (Sum Int) (StateT String [])

runParser :: Parser a -> String -> [(a, Int)]
runParser p s = [ (x,n) | ((x, Sum n), "") <- runStateT (runWriterT p) s ]

parse_char :: Parser Char
parse_char = do
  c:cs <- lift get
  lift (put cs)
  return c

tick :: Parser ()
tick = tell (Sum 1)


