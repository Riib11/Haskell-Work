module Ledger where

--------------------
---- Ledger
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
transact addFrom addTo amt led =
        addStatement newTo $ addStatement newFrom led
        where newTo   = Statement addTo   (amtTo - amt)     -- new statement for receiver
              newFrom = Statement addFrom (amtFrom   + amt) -- new statement for sender
              amtTo   = getAmount addTo   led               -- new amount for reciever
              amtFrom = getAmount addFrom led               -- new amount for sender