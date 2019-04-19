-- Demonstration: parsing monadically in Haskell
-- Source: Hutton G - Monadic Parsing in Haskell

{-# LANGUAGE ScopedTypeVariables #-}

-- ParseResults

newtype ParseResults a = ParseResults [(a, String)]

instance Functor ParseResults where
  f `fmap` ParseResults rs = ParseResults $ zip (f `map` xs) ss where (xs, ss) = unzip rs

instance Monoid (ParseResults a) where
  mempty = ParseResults []
  ParseResults rs `mappend` ParseResults rs' = ParseResults $ rs ++ rs'
  

-- Parser

newtype Parser a = Parser (String -> ParseResults a)

instance Functor Parser where
  f `fmap` Parser pa = Parser $ \s -> let rs = pa s in (f `fmap` rs)

instance Applicative Parser where
  pure x = Parser $ \s -> ParseResults [(x, s)]
  Parser pf <*> Parser pa = Parser $
    \s -> let ParseResults rsf = pf s in mconcat $ (\(f, s) -> f `fmap` pa s) `map` rsf

instance Monad Parser where
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  Parser pa >>= a_pb = Parser $
    \s -> let rsa = pa s ; rsb = mconcat $ (\(a, s') -> a_pb a) `map` rsa in _

-- Parser: Examples

item :: Parser Char
item = Parser f where
  f ""     = ParseResults []
  f (c:cs) = ParseResults [(c, cs)]
