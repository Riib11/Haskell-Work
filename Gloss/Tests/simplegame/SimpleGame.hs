-- import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- world state
data World = State1 | State2 | State3 | State4
instance Show World where
    show State1 = "State1"
    show State2 = "State2"
    show State3 = "State3"
    show State4 = "State4"


-- simple testing world state update
nextState :: World -> World
nextState State1 = State2
nextState State2 = State3
nextState State3 = State4
nextState State4 = State1


-- main
main = play window background_color rate initial_conditions render input update


-- display mode (window)
window :: Display
window =
    (InWindow
        "Simple Game"   -- window title
        (600, 600)      -- window size
        (10,  10))      -- window position


-- window background color
background_color :: Color
background_color = black


-- updates per second
rate :: Int
rate = 5


-- initial world state
initial_conditions :: World
initial_conditions =
    State1


-- function to convert world to picture
render :: World -> Picture
render w =
    Color white
    $ Translate (-170) (-20)
    $ Scale 0.5 0.5
    $ Text
    $ show w


-- handle input events
input :: Event -> World -> World
input (EventKey key key_state modifiers (x,y)) w = w
input (EventMotion (x, y)) w = w
input _ w = w


-- update called each simulation step
update :: Float -> World -> World
update dt w =
    nextState w