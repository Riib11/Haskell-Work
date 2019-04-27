module RegularExpression where

import Parser
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Char

-----------------------------------------------------------------------------------------------------------------------------
-- Regular Expression
-----------------------------------------------------------------------------------------------------------------------------

data RegularExpression
  = EmptySet
  | EmptyString
  | Literal       Char
  | Concatenation RegularExpression RegularExpression
  | Alteration    RegularExpression RegularExpression
  | KleeneStar    RegularExpression

instance Semigroup RegularExpression where
  e <> EmptySet = e
  e <> e'       = Concatenation e e'

instance Monoid RegularExpression where
  mempty = EmptySet

instance Show RegularExpression where
  show EmptySet             = "{}"
  show EmptyString          = "ε"
  show (Literal       c)    = [c]
  show (Concatenation e e') = show e ++ show e'
  show (Alteration    e e') = show e ++ "|" ++ show e'
  show (KleeneStar    e)    = case e of { (Literal c) -> [c] ++ "*" ; _ -> "(" ++ show e ++ ")*" }

type RegEx = RegularExpression

-------------------------------------------
-- Empty Set

empty_set :: Parser RegEx
empty_set = return EmptySet

-------------------------------------------
-- Empty String

empty_string :: Parser RegEx
empty_string = return EmptyString

-------------------------------------------
-- Literal

literal :: Char -> Parser RegEx
literal c = do
  cs <- get
  case cs of
    []     -> mempty
    c':cs' -> if c == c'
      then put cs' >> return (Literal c)
      else mempty

-------------------------------------------
-- Concatenation

concatenation :: Parser RegEx -> Parser RegEx -> Parser RegEx
concatenation p p' = do { e <- p ; e' <- p'
  ; return $ Concatenation e e' }

-------------------------------------------
-- Alteration

alteration :: Parser RegEx -> Parser RegEx -> Parser RegEx
alteration p p' = do { e <- p +++ p' ; return e }

-------------------------------------------
-- Kleene Star

kleene_star :: Parser RegEx -> Parser RegEx
kleene_star p = let
  rec = do { e <- p ; es <- rec ; return $ e:es } +++ return []
  in do
    es <- rec
    case es of
      [] -> mempty
      e:es -> return $ KleeneStar e


-----------------------------------------------------------------------------------------------------------------------------
-- Examples
-----------------------------------------------------------------------------------------------------------------------------

-- run an example, say example1, via
-- > runParser example1 "ab"

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
-- example1 = alteration a (kleene_star b)

-- (a|b)*
example2 = star (a ||| b)
-- example2 = kleene_star (alteration a b)

-- ab*(c|ε)
example3 = a &&& (star b) &&& (c ||| e)
-- example3 = concatenation a (concatenation (kleene_star b) (alteration c e))

-- (a|(b(ab*a)*b))*
example4 = star (a ||| (b &&& star (a &&& star b &&& a) &&& b))
-- example4 = kleene_star (alteration a (concatenation b (concatenation (kleene_star (concatenation a (concatenation (kleene_star b) a))) b)))




--
