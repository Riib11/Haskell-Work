import Ledger

---------------------
---- example
---------------------

addA = "addA" :: Address
amtA = 10 :: Amount
stmA = Statement addA amtA

addB = "add2" :: Address
amtB = 20 :: Amount
stmB = Statement addB amtB

-- example ledger (initializing)
ledger1 = addStatement stmA $ addStatement stmB EmptyNode

-- example transaction: send all A's money to B!
ledger2 = transact addA addB amtA ledger1