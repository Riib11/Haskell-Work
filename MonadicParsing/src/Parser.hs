{-# LANGUAGE UndecidableInstances #-}

module Parser where

import Data.Semigroup
import qualified Data.List.NonEmpty
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import qualified Data.Char

-----------------------------------------------------------------------------------------------------------------------------
-- Parser
-----------------------------------------------------------------------------------------------------------------------------

type Parser a = StateT String [] a

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT

-- Semigroup (StateT s m a)

instance (Monad m, Monoid (m (a, s))) => Semigroup (StateT s m a) where
  st <> st' = StateT mempty
  sconcat = mconcat . Data.List.NonEmpty.toList
  stimes i st = mconcat $ Data.List.NonEmpty.take (fromIntegral i) $ Data.List.NonEmpty.repeat st

-- Monoid (StateT s m a)

instance (Monad m, Monoid (m (a, s))) => Monoid (StateT s m a) where
  mempty         = StateT mempty
  mappend st st' = StateT $ \s -> let mas = runStateT st s ; ma's = runStateT st' s in mappend mas ma's

(+++) :: Parser a -> Parser a -> Parser a
(+++) = mappend

-----------------------------------------------------------------------------------------------------------------------------
-- First Order Combinators
-----------------------------------------------------------------------------------------------------------------------------

char_predicate :: (Char -> Bool) -> Parser Char
char_predicate p = do
  c:cs <- get
  if p c
    then put cs >> return c
    else lift []

char :: Char -> Parser Char
char c = char_predicate (== c)

string :: String -> Parser String
string s = case s of
  ""   -> return ""
  c:cs -> char c >> string cs

-----------------------------------------------------------------------------------------------------------------------------
-- Second Order Combinators
-----------------------------------------------------------------------------------------------------------------------------

option :: Parser a -> Parser b -> Parser b -> Parser b
option opt p p' = do { opt ; p } +++ p'

options :: [(Parser a, Parser b)] -> Parser b -> Parser b
options optps p' = foldl (+++) p' $ map (\(opt, p) -> opt >> p) optps

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do { a <- p ; as <- many p ; return (a:as) }

seperated_by :: Parser a -> Parser b -> Parser [a]
p `seperated_by` sep = (p `seperated_by1` sep) +++ return []

seperated_by1 :: Parser a -> Parser b -> Parser [a]
p `seperated_by1` sep = do { a <- p ; as <- many (sep >> p) ; return (a:as) }

chain_left :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chain_left p op a = (p `chain_left1` op) +++ return a

chain_left1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chain_left1` op = do { a <- p ; rest a } where
  rest a = do { f <- op ; b <- p ; rest (f a b) } +++ return a

-----------------------------------------------------------------------------------------------------------------------------
-- Lexical Combinators
-----------------------------------------------------------------------------------------------------------------------------

isSpace :: Char -> Bool
isSpace c = case c of
  ' '  -> True
  '\n' -> True
  _    -> False

space :: Parser String
space = many $ char_predicate isSpace

token :: Parser a -> Parser a
token p = do { a <- p ; space ; return a }

symbol :: String -> Parser String
symbol str = token (string str)
