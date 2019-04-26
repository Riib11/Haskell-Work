module Lambda1 where

import Parser

{- Lambda1 ---------------------------------------------------------------------------------------------------------------

This language implements a simple, untyped lambda calculus.
The only term atoms avaliable are A, B, and C.
Structures are right-associative by default, and allows explicit association.
Note that spaces are required between every token! (including parentheses)

program ::= expr*

expr ::= (expr)
       | let name := expr in expr # substitution
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
expression =
      substitution
  +++ function
  +++ application
  +++ atom
  +++ associated expression

expression_left :: Parser Expression
expression_left = substitution
              +++ function
              +++ atom
              +++ associated expression_left

              associated :: Parser a -> Parser a
              associated p = do { symbol "(" ; a <- p ; symbol ")" ; return a }

-------------------------------------

substitution :: Parser Expression
substitution = do { symbol "let" ; n <- name ; symbol ":=" ; x <- expression ; symbol "in" ; y <- expression
                  ; return $ Substitution n x y }

function :: Parser Expression
function = do { symbol "function" ; n <- name ; symbol "=>" ; x <- expression
              ; return $ Function n x }

application :: Parser Expression
application = do { x <- expression_left ; y <- expression
                 ; return $ Application x y }

atom :: Parser Expression
atom = do { n <- name
          ; return $ Atom n }

-----------------------------------------------------------------------------
-- Name

data Name = A | B | C
  deriving (Show)

name :: Parser Name
name = do { symbol "A" ; return A }
   +++ do { symbol "B" ; return B }
   +++ do { symbol "C" ; return C }
