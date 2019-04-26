module SimpleLang04 where

import Control.Monad.Trans.Class
import Parser
import Data.Char (isDigit)

{- Language 3 ---------------------------------------------------------------------------------------------------------------

<!--
  This language implements a simple integer algebra
  along with a `let ... in ...` construction
-->

  program ::= expr
     expr ::= value | let name := expr in expr
      int ::= [+|-]?nat | int binop int
    binop ::= + | - | * | /
      nat ::= digit*
    digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

----------------------------------------------------------------------------------------------------------------------------}

-- data Expression =
--
-- program :: Parser Int
-- program = expr
--
-- expr :: Parser Int
-- expr = do { symbol "let" ; xn <- name ; symbol ":=" ; xe <- expr ; symbol "in" ; e <- expr ; return (substitute xe xn e) }
--
-- substitute :: Name -> Int ->
--
--
-- expr = term `chain_left1` addop
--
-- term :: Parser Int
-- term = factor `chain_left1` mulop
--
-- factor :: Parser Int
-- factor = int +++ do { symbol "(" ; n <- expr ; symbol ")" ; return n }
--
-- int :: Parser Int
-- int = options [ (symbol "+", nat), (symbol "-", do { n <- nat ; return (-n) }) ] nat
--
-- nat :: Parser Int
-- nat = many1 digit >>= return . foldl (\n d -> n * 10 + d) 0
--
-- -- digit :: Parser Int
-- -- digit = options digit_optps (lift []) where
-- --   digit_optps = (map (\c -> (symbol [c], return $ read [c])) ['0'..'9'])
--
-- digit :: Parser Int
-- digit = do { x <- token (char_predicate isDigit) ; return (read [x]) }
--
-- addop :: Parser (Int -> Int -> Int)
-- addop = do { symbol "+" ; return (+) } +++ do { symbol "-" ; return (-) }
--
-- mulop :: Parser (Int -> Int -> Int)
-- mulop = do { symbol "*" ; return (*) } +++ do { symbol "/" ; return (div) }
