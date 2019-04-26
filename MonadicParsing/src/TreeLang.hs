module TreeLang where

import Control.Monad.Trans.Class
import Parser
import Data.Char (isDigit)

{- TreeLang -----------------------------------------------------------------------------------------------------------------

<!--
  This language implements a simple parser for grammar trees,
  with function application and `let in` blocks.
-->

program ::= term

term ::= function name => term    # term function
       | apply term to term       # term application
       | let name := term in term # term substitution
       | name                     # name atom

name ::= a | b | c

----------------------------------------------------------------------------------------------------------------------------}
-- Program

program :: Parser Term
program = term

------------------------------------------------
-- Term

data Term
  = TermFunction     Name Term
  | TermApplication  Term Term
  | TermSubstitution Name Term Term
  | TermName         Name
  deriving (Show)

term :: Parser Term
term =
  term_function +++
  term_substitution +++
  term_application +++
  term_name

term_function :: Parser Term
term_function = do
  symbol "function"
  n <- name
  symbol "=>"
  t <- term
  return $ TermFunction n t

term_application :: Parser Term
term_application = do
  symbol "apply"
  t1 <- term
  symbol "of"
  t2 <- term
  return $ TermApplication t1 t2

term_substitution :: Parser Term
term_substitution = do
  symbol "let"
  xn <- name
  symbol ":="
  xt <- term
  symbol "in"
  t <- term
  return $ TermSubstitution xn xt t

term_name :: Parser Term
term_name = do
  n <- name
  return $ TermName n

------------------------------------------------
-- Name

data Name
  = Name String
  deriving (Show)

name :: Parser Name
name =
  do { symbol "a" ; return $ Name "a" } +++
  do { symbol "b" ; return $ Name "b" } +++
  do { symbol "c" ; return $ Name "c" }
-- name = options (map (\c -> (symbol c, return $ Name c) ["a", "b", "c"])) (lift [])


----------------------------------------------------------------------------------------------------------------------------}
