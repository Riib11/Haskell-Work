--------------------
-- types
--------------------

type Address = String
type Amount = Int

-- data Statement = { address::Address, amount::Amount } deriving(Show)
data Statement = Statement Address Amount deriving(Show)

-- data Transaction = Transaction { source::Statement, target::Statement } deriving(Show)
data Transaction = Transaction Statement Statement deriving(Show)

-- data Ledger = Ledger { transactions::Transaction } deriving(Show)
data Ledger = Ledger [Statement]

--------------------
-- functions
--------------------

getAmount :: Ledger -> Address -> Amount
getAmount led add = -- loop through all statements to find what the total is for an address

createTransaction :: Address -> Amount -> Address
createTransaction add1 amt add2 =
    Transaction (Statement add2 (amt1-amt)) (Statement add1 (amt2+amt))
    where amt1 = getAmount add1
          amt2 = getAmount add2

addTransaction :: Ledger -> Transaction -> Ledger
addTransaction (Ledger ts) transaction = Ledger (transaction:ts)

--------------------
-- example
--------------------

-- A
add1 = "a" :: Address
amt1 = 1
-- B
add2 = "b" :: Address
amt2 = 0

