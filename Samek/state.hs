module State
( State(notes, running), state_start, show_notes
, Command, string_to_command
, update
) where

import UtilString
import UtilList

-- +=========================
-- | State
-- +=========================
data State = State
    { world    :: World
    , agents   :: [Agent]
    , objects  :: [Object]
    , time     :: Time
    , running  :: Bool
    , notes    :: [StateNote]
    } deriving (Show)

type Entity = Either Object Agent
type Time = Int

state_start :: State
state_start = State
    world_start     -- world
    agents_start    -- agents
    objects_start   -- objects
    0               -- time
    True            -- running
    notes_start     -- notes

-- State: setters

type StateSetter a = State -> a -> State

set_world :: StateSetter World
set_world (State _ as os t r ns) w = State w as os t r ns

set_agents :: StateSetter [Agent]
set_agents (State w _ os t r ns) as = State w as os t r ns

set_objects :: StateSetter [Object]
set_objects (State w as _ t r ns) os = State w as os t r ns

set_time :: StateSetter Time
set_time (State w as os _ r ns) t = State w as os t r ns

set_running :: StateSetter Bool
set_running (State w as os t _ ns) r = State w as os t r ns

set_notes :: StateSetter [StateNote]
set_notes (State w as os t r _) ns = State w as os t r ns

-- State: modifiers

add_note :: State -> StateNote -> State
add_note state n = set_notes state $ (notes state) ++ [n]

-- +=========================
-- | World
-- +=========================
data World = World
    { bounds :: (Int,Int,Int)
    } deriving (Show)

world_start = World (10, 10, 10)

-- +=========================
-- | StateNote
-- +=========================
data StateNote
    = StateNoteError String
    | StateNoteClear
    | StateNoteMessage String String
    | StateNoteString String String
    -- | StateNoteTable [String] [[String]]

instance Show StateNote where
    show sn = case sn of
        StateNoteError s     -> "[!!] " ++ s
        StateNoteClear       -> ""
        StateNoteMessage t s -> s
        StateNoteString  t s -> s
        -- | StateNoteTable hs xss ->

notes_start = [StateNoteMessage "this is the start state and this is the start state message. Its a little long because I wanted to see how it would handle long messages in the textbok sort of thing."]

show_notes :: State -> String
show_notes state = foldr
    (\a b -> (show a) ++ "\n" ++ b) "" (notes state)

-- show_notes :: State -> String
-- show_notes s = helper $ notes s
--     where
--         helper :: [StateNote] -> String
--         helper [] = ""
--         helper (s:ss) = (show s) ++ "\n" ++ (helper ss)

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

objects_start = []

-- +=========================
-- | Agent
-- +=========================
data Agent = Agent
    { agent_uid :: UId
    , act :: State -> State }

instance Show Agent where
    show agent = show $ "Agent<"++(agent_uid agent)++">"

agents_start = []

-- +=========================
-- | Command (from User)
-- +=========================

-- TODO
data Command
    = Echo String
    | Quit
    | Clear
    | Unrecognized String

-- TODO
string_to_command :: String -> Command
string_to_command [] = Unrecognized "<empty>"
string_to_command str
    | cmd == "echo" = Echo
        (foldr (\a b-> a ++ " " ++ b) "" args)
    | cmd == "clear" = Clear
    | cmd == "quit" = Quit
    | otherwise = Unrecognized cmd
    where
        raw = UtilString.split ' ' str
        (cmd, args) = case UtilList.pop raw of
            Nothing -> ("", [])
            Just (c,as) -> (c, as)

-- TODO
apply_command :: State -> Command -> State
apply_command state command =
    case command of
        Echo note -> add_note state (StateNoteMessage "Echo" note)
        Quit -> set_running state False
        Clear -> set_notes state []
        Unrecognized cmd -> add_note state (StateNoteError "[Error]" $ "unrecognized command: " ++ cmd)

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