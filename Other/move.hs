import Control.Monad

startMap = Map (('#',' '),(' ','#'))
otherMap = Map (('Q','W'),('E','R'))

newtype Map = Map ((Char,Char),(Char,Char))

s = ' '
nl = '\n'
instance Show Map where
    show (Map ((a,b),(d,e))) = a:s:b:nl:d:s:e:[]

move :: Map -> Char -> Map
move (Map ((a,b),(c,d))) 'a' = Map ((c,d),(a,b))
move (Map ((a,b),(c,d))) 's' = Map ((a,c),(b,d))
move (Map ((a,b),(c,d))) 'd' = Map ((a,d),(b,c))
move (Map ((a,b),(c,d))) 'f' = Map ((c,a),(d,b))
move map _   = map

play :: Map -> IO ()
play map = do
    putStrLn ""
    putStrLn $ show map
    putStrLn ""
    c <- getChar
    putStrLn ""
    when (c /= 'q') $ do
        play $ move map c