module State
( State(message), start_state
, Command, string_to_command
, update
) where

-- +=========================
-- | State
-- +=========================
data State = State
    { agents   :: [Agent]
    , objects  :: [Object]
    , time     :: Time
    , running  :: Bool
    , message  :: String
    } deriving (Show)

start_state :: State
start_state = State [] [] 0 True
    "this is the start state and this is the start state message. Its a little long because I wanted to see how it would handle long messages in the textbok sort of thing."

type Entity = Either Object Agent
type Time = Int

type StateSetter a = State -> a -> State

set_agents :: StateSetter [Agent]
set_agents (State _ os t r m) as = State as os t r m

set_objects :: StateSetter [Object]
set_objects (State as _ t r m) os = State as os t r m

set_time :: StateSetter Time
set_time (State as os _ r m) t = State as os t r m

set_running :: StateSetter Bool
set_running (State as os t _ m) r = State as os t r m

set_message :: StateSetter String
set_message (State as os t r _) m = State as os t r m

-- +=========================
-- | Unique Identifier (UId)
-- +=========================
type UId = String

-- +=========================
-- | Object
-- +=========================
data Object = Object
    { object_uid :: UId }

instance Show Object where
    show object = show $ "Object<"++(object_uid object)++">"

-- +=========================
-- | Agent
-- +=========================
data Agent = Agent
    { agent_uid :: UId
    , act :: State -> State }

instance Show Agent where
    show agent = show $ "Agent<"++(agent_uid agent)++">"

-- +=========================
-- | Command (from User)
-- +=========================

-- TODO
data Command = Command { raw :: String }

-- TODO
string_to_command :: String -> Command
string_to_command str = Command str

-- TODO
apply_command :: State -> Command -> State
apply_command state command = set_message state (raw command)

-- +=========================
-- | Updating
-- +=========================

inc_time :: State -> State
inc_time state = set_time state (time state + 1)

update_agents :: State -> [Agent] -> [Agent] -> State
update_agents state agents agents_done =
    case agents of
        [] -> state
        (a:as) -> update_agents
            (act a state)     -- agents acts on state
            (tail agents)     -- removed this agent from queue
            (a : agents_done) -- designate this agent as done

-- 1. user command
-- 2. agents actions
-- 3. increment time
update :: State -> Command -> State
update state command = state3
    where
        state1 = apply_command state command
        state2 = update_agents state1 (agents state) []
        state3 = inc_time state2


-- +=========================
-- | Start State
-- +=========================

-- TODO