module RegularExpression where

import Parser
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Char

-----------------------------------------------------------------------------------------------------------------------------
-- Regular Expression
-----------------------------------------------------------------------------------------------------------------------------

-------------------------------------------
-- Empty Set

empty_set :: Parser ()
empty_set = mempty

-------------------------------------------
-- Empty String

empty_string :: Parser ()
empty_string = return ()

-------------------------------------------
-- Literal

literal :: Char -> Parser ()
literal c = do
  cs <- get
  case cs of
    []     -> mempty
    c':cs' -> if c == c'
      then put cs' >> return ()
      else mempty

-------------------------------------------
-- Concatenation

concatenation :: Parser () -> Parser () -> Parser ()
concatenation = (>>)

-------------------------------------------
-- Alteration

alteration :: Parser () -> Parser () -> Parser ()
alteration = (+++)

-------------------------------------------
-- Kleene Star

kleene_star :: Parser () -> Parser ()
kleene_star p = (p >> kleene_star p) +++ return ()

-----------------------------------------------------------------------------------------------------------------------------
-- Examples
-----------------------------------------------------------------------------------------------------------------------------

-- run an example, say example1, via
-- > runParser example1 <input_string>
-- if it evaluates to a non-empty list, then the input string parses
-- if it evaluates to the empty list, then the input does not parse

-- a, b, c
[a, b, c] = map literal ['a', 'b', 'c']

-- ε
e = empty_string

-- concatenation
(&&&) = concatenation

-- alteration
(|||) = alteration

--- kleene star
star = kleene_star

-- a|b*
example1 = a ||| (star b)

-- (a|b)*
example2 = star (a ||| b)

-- ab*(c|ε)
example3 = a &&& (star b) &&& (c ||| e)

-- (a|(b(ab*a)*b))*
example4 = star (a ||| (b &&& star (a &&& star b &&& a) &&& b))
