liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = mx >>= (\x -> return (f x))

liftM2 :: Monad m => (a -> b) -> m a -> m b
liftM2 f action = do
    x <- action
    return (f x)

main = (readFile "monads.hs") >>= putStrLn