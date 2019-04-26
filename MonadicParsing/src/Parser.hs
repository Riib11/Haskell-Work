{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser where

import qualified Data.Semigroup as Semigroup
import qualified Data.List.NonEmpty as NonEmpty

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Char

-----------------------------------------------------------------------------------------------------------------------------
-- Parser
-----------------------------------------------------------------------------------------------------------------------------

type Parser a = StateT String [] a

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT

(+++) :: Parser a -> Parser a -> Parser a
(+++) = mappend

instance (Monad m, Monoid (m a), Monoid (m (a, s))) => Semigroup (StateT s m a) where
  st <> st' = StateT $ \s -> let (as, as') = (runStateT st s, runStateT st' s) in as `mappend` as'

instance (Monad m, Monoid (m a), Monoid (m (a, s))) => Monoid (StateT s m a) where
  mempty = lift mempty

-----------------------------------------------------------------------------------------------------------------------------
-- First Order Combinators
-----------------------------------------------------------------------------------------------------------------------------

char_predicate :: (Char -> Bool) -> Parser Char
char_predicate p = do { c:cs <- get ; if p c then put cs >> return c ; else lift [] }

char :: Char -> Parser Char
char c = char_predicate (== c)

string :: String -> Parser String
string s = case s of { "" -> return "" ; c:cs -> char c >> string cs }

-----------------------------------------------------------------------------------------------------------------------------
-- Second Order Combinators
-----------------------------------------------------------------------------------------------------------------------------

optional :: Parser a -> Parser (Maybe a)
optional p = do { a <- p ; return (Just a) } +++ return Nothing

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

check :: Parser a -> Parser ()
check p = p >> return ()

-----------------------------------------------------------------------------------------------------------------------------
-- Lexical Combinators
-----------------------------------------------------------------------------------------------------------------------------

end :: Parser ()
end = do { s <- get ; case s of { "" -> return () ; _ -> lift [] } }

space :: Parser ()
space = check (many1 $ char_predicate isSpace) +++ end

token :: Parser a -> Parser a
token p = do { a <- p ; space ; return a }

symbol :: String -> Parser String
symbol = token . string
