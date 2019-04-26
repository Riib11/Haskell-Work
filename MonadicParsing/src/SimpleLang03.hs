module SimpleLang03 where

import Control.Monad.Trans.Class
import Parser
import Data.Char (isDigit)

{- Language 3 ---------------------------------------------------------------------------------------------------------------

<!-- TODO: description -->
<!-- This language implements simple integer algebra.
Addition, subtraction, multiplication and integer division are supported,
with proper implicit associativity and optional explicit associativity. -->

  program ::= expr
     expr ::= term   | expr addop term
     term ::= factor | term mulop factor
   factor ::= nat    | (expr)

  int   ::= [+|-]?nat
  nat   ::= digit+
  digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  addop ::= + | -
  mulop ::= * | /

----------------------------------------------------------------------------------------------------------------------------}

program :: Parser Int
program = expr

expr :: Parser Int
expr = term `chain_left1` addop

term :: Parser Int
term = factor `chain_left1` mulop

factor :: Parser Int
factor = int +++ do { symbol "(" ; n <- expr ; symbol ")" ; return n }

int :: Parser Int
int = options [ (symbol "+", nat), (symbol "-", do { n <- nat ; return (-n) }) ] nat

nat :: Parser Int
nat = many1 digit >>= return . foldl (\n d -> n * 10 + d) 0

-- digit :: Parser Int
-- digit = options digit_optps (lift []) where
--   digit_optps = (map (\c -> (symbol [c], return $ read [c])) ['0'..'9'])

digit :: Parser Int
digit = do { x <- token (char_predicate isDigit) ; return (read [x]) }

addop :: Parser (Int -> Int -> Int)
addop = do { symbol "+" ; return (+) } +++ do { symbol "-" ; return (-) }

mulop :: Parser (Int -> Int -> Int)
mulop = do { symbol "*" ; return (*) } +++ do { symbol "/" ; return (div) }
