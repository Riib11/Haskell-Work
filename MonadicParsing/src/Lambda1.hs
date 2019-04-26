module Lambda1 where

import Parser

{- Language 1 ---------------------------------------------------------------------------------------------------------------

This language implements simple algebra with the only explicit naturals
available as those less than 10 (because only one digit is read per number).
Addition, subtraction, multiplication and integer division are supported,
with proper implicit associativity and optional explicit associativity.

program ::= expr*

expr ::= let name := expr in expr # substitution
       | function name => expr # function
       | apply expr expr # application
       | name # atom

name ::= a | b | c

----------------------------------------------------------------------------------------------------------------------------}

-----------------------------------------------------------------------------

program :: Parser Expression
program = expression

-----------------------------------------------------------------------------
-- Expression

data Expression
  = Substitution Name Expression Expression
  | Function Name Expression
  | Application Expression Expression
  | Atom Name

instance Show Expression where
  show (Substitution n x y) = "(let " ++ show n ++ " := " ++ show x ++ " in " ++ show y ++ ")"
  show (Function n x)       = "(function " ++ show n ++ " => " ++ show x ++ ")"
  show (Application x y)    = "(" ++ show x ++ " " ++ show y ++ ")"
  show (Atom n)             = show n

expression :: Parser Expression
expression = substitution +++ function +++ application +++ atom

expression_left :: Parser Expression
expression_left = substitution +++ function +++ atom

substitution :: Parser Expression
substitution = do { symbol "let" ; n <- name ; symbol ":=" ; x <- expression ; symbol "in" ; y <- expression
  ; return $ Substitution n x y }

function :: Parser Expression
function = do { symbol "function" ; n <- name ; symbol "=>" ; x <- expression
  ; return $ Function n x }

application :: Parser Expression
application = do { x <- expression_left ; symbol "$" ; y <- expression
  ; return $ Application x y }

atom :: Parser Expression
atom = do { n <- name ;
  return $ Atom n }

-----------------------------------------------------------------------------
-- Name

data Name = A | B | C
  deriving (Show)

name :: Parser Name
name =
  do { symbol "a" ; return A } +++
  do { symbol "b" ; return B } +++
  do { symbol "c" ; return C }
