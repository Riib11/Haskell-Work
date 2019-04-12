module Blackjack () where

type Card = Int

data State = State
    { cards :: [Card]
    , terminal :: Bool }
    deriving (Show)

cardsum :: State -> Int
cardsum state = sum $ cards state

addcard :: State -> Card -> State
addcard state card = State (card : (cards state)) (terminal state)

-- compare state to simulated dealer play
compare_dealer :: State -> Float
compare_dealer state = 1.0

data Action = Hit | Stay

data Transition = Transition
    { source :: State
    , target :: State
    , probability :: Float }

-- get all transitions from a state
get_transitions :: State -> [Transition]
get_transitions state =
    if (terminal state) then [] -- terminal
    else if (cardsum state) >= 21 then [] -- busted
    else generate_transitions 10
        where
            generate_transitions :: Card -> [Transition]
            generate_transitions 0 = []
            generate_transitions 10 = (Transition state (addcard state 10) (4 / 13))
                : generate_transitions (10 - 1)
            generate_transitions i = (Transition state (addcard state i) (1 / 13))
                : generate_transitions (i - 1)

-- get utility of a state
get_utility :: State -> Float
get_utility state =
    if (terminal state) then
        if (cardsum state) > 21 then 0.0
        else compare_dealer state
    else 0.0

main = do putStrLn "hello world"