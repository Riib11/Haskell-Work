data State = State
  { cards  :: [Card]   -- All the Cards in this game
  , phase  :: Phase    -- Current Phase of the game
  , stack  :: [Action] -- Actions that are waiting to resolve
}

resolve_stack :: State -> State
resolve_stack state =
  case apply_effect (state_new) (head (stack state)) of
    Just state_new -> state_new -- enacted action
    Nothing -> state            -- action condition not satisfied

-- Phases in which actions can happen
data Phase
  = Begin
  | Main1
  | Combat
  | Main2
  | End
  deriving (Show)

-- Zones that cards can be in
data Zone
  = Deck
  | Hand
  | Play
  | Grave
  | Exile
  | Hidden
  | Removed
  deriving (Show)

-- Describes a unit instance of State change
-- caused by a single source.
data Action = Action
  { condition :: Condition
  , effect    :: Effect
}

type Condition = State -> Bool -- requirements in order to play Actions
type Effect = [State -> State] -- how actions change the State

is_playable :: State -> Action -> Bool
is_playable state action = condition action state

apply_effect :: State -> Effect -> State
apply_effect state (e:es) = apply_effect (e state) es
apply_effect state _      = state

play_action :: State -> Action -> Maybe State
play_action state action =
  if (condition action state)
      then Just (apply_effect state (effect action))
      else Nothing

data Card = Card
  { move :: Zone -> Zone -> Action -- describes how to move this card around
  , zone :: Zone                   -- the current zone of this card
  , name :: String                 -- the name of this card
}

is_movable :: State -> Zone -> Zone -> Card -> Bool
is_movable state source target card = is_playable state (move card source target)