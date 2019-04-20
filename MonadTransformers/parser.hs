{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Trans.State
import Control.Monad.Trans.Class

type Parser a = StateT String [] a

run_parser :: Parser a -> String -> [(a, String)]
run_parser = runStateT

instance (Monad m, Monoid (m (a, s))) => Monoid (StateT s m a) where
  mempty = StateT mempty
  mappend st st' = StateT $ \s -> let mxs = runStateT st s ; mxs' = runStateT st' s in mappend mxs mxs'

parse_char_predicate :: (Char -> Bool) -> Parser Char
parse_char_predicate p = do
  c:cs <- get
  if p c
    then put cs >> return c
    else lift []

parse_char :: Char -> Parser Char
parse_char c = parse_char_predicate (== c)

parse_string :: String -> Parser String
parse_string s = case s of
  ""   -> return ""
  c:cs -> parse_char c >> parse_string cs

-- many :: Parser a -> Parser [a]
-- many p = many1 ++ 
