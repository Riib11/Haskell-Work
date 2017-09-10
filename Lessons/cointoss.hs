import System.Random
import Control.Monad.State

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen1 =
    let (coin1,gen2) = random gen1
        (coin2,gen3) = random gen2
        (coin3,gen4) = random gen3
    in (coin1,coin2,coin3)

type Stack = [Int]

-- pop :: Stack -> (Int, Stack)
-- pop (x:xs) = (x,xs)

-- push :: Int -> Stack -> ((),Stack)
-- push a xs = ((),a:xs)

-- stackManip :: Stack -> (Int, Stack)
-- stackManip stack =
--     let ((),newStack1) = push 3 stack
--         (a,newStack2) = pop newStack1
--     in pop newStack2

-- now, State monad
-- newtype State s a = State { runState::s->(a,s) }

-- pop :: State Stack Int
-- pop = State $ \(x:xs) -> (x,xs)

-- push :: Int -> State Stack ()
-- push a = State $ \xs -> ((),a:xs)

-- stackManip :: State Stack Int
-- stackManip = do
--     push 3
--     a <- pop
--     pop

randomSt :: (RandomGen g, Random r) => State g r
randomSt = State random

threeCoins' :: State StdGen (Bool,Bool,Bool)
threeCoins' = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a,b,c)