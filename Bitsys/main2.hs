--------------------
---- types
--------------------

data Ledger = Node Statement Ledger | EmptyNode deriving(Show)
data Statement = Statement Address Amount deriving(Show)

type Address = String
type Amount = Int

--------------------
---- functions
--------------------

getAmount :: Address -> Ledger -> Amount
getAmount add (Node (Statement address amount) led)
    | add == address    = amount
    | otherwise         = getAmount add led -- try next node
getAmount add EmptyNode = 0

addStatement :: Statement -> Ledger -> Ledger
addStatement stm led = Node stm led

transact :: Address -> Address -> Amount -> Ledger -> Ledger
transact add1 add2 amt led =
    addStatement (Statement add2 ((getAmount add2 led) + amt)) $ addStatement (Statement add1 ((getAmount add1 led) - amt)) led

negative :: Int -> Int
negative a = 0 - a

--------------------
---- example
--------------------

add1 = "00000000" :: Address
amt1 = 10 :: Amount
stm1 = Statement add1 amt1

add2 = "11111111" :: Address
amt2 = 0 :: Amount
stm2 = Statement add2 amt2

ledger = addStatement stm1 $ addStatement stm2 EmptyNode