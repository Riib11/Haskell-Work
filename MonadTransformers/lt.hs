data Lt i a
  = Pure a
  | Get (i -> Lt i a)

ask :: Lt i i
ask = Get Pure

instance Functor (Lt i) where
  fmap = undefined

instance Applicative (Lt i) where
  pure = undefined
  (<*>) = undefined

instance Monad (Lt i) where
  return = Pure
  Pure x >>= k = k x
  Get k' >>= k = Get (k' >>> k)

(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f
