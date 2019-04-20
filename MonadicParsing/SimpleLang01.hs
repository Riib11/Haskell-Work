module SimpleLang01 (main) where

import Parser
import Data.Char hiding (isSpace)

{- Language 1 ---------------------------------------------------------------------------------------------------------------

    expr ::= expr addop term | term
    term ::= term mulop factor | factor
  factor ::= digit | (expr)

  digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  addop ::= + | -
  mulip ::= * | /

----------------------------------------------------------------------------------------------------------------------------}

expr :: Parser Int
expr = term `chain_left1` addop

term :: Parser Int
term = factor `chain_left1` mulop

factor :: Parser Int
factor = digit +++ do { symbol "(" ; n <- expr ; symbol ")" ; return n }

digit :: Parser Int
digit = do { x <- token (char_predicate isDigit) ; return (ord x - ord '0') }

addop :: Parser (Int -> Int -> Int)
addop = do { symbol "+" ; return (+) } +++ do { symbol "-" ; return (-) }

mulop :: Parser (Int -> Int -> Int)
mulop = do { symbol "*" ; return (*) } +++ do { symbol "/" ; return (div) }

-----------------------------------------------------------------------------------------------------------------------------
-- Main
-----------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  input <- getLine
  let (output, rest) = head $ runParser expr input
  putStrLn $ "=> " ++ show output
  if rest /= ""
    then putStrLn $ "(unparsed: " ++ show rest ++ ")"
    else putStr ""

