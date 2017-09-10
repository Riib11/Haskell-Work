import Data.Monoid
import Control.Monad.Writer

inBig :: Int -> (Bool,String)
inBig x = (x > 9,"compared to 9")

applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

type Price = Sum Int

a = Sum 1
b = Sum 10

-- Writer is defined like so:
-- newtype Writer w a = Writer { runWriter:: (a,w) }
-- instance (Monoid w) => Monad (Writer w) where
--     return x = Writer (x,mempty)
--     (Writer (x,v)) >>= f = let (Writer (y,v')) = f x in Writer (y,mappend v v')

-- application
logNumber :: Int -> Writer [String] Int  
logNumber x = Writer ["Got number: " ++ show x] x
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b) 