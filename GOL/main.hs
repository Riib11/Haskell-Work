import Graphics.Gloss.Interface.Pure.Game

{- /------------------------/
  /  Game of Life          /
 /------------------------ 
/  An implementation of
   Conway's game of life
   in haskell.            /
 ------------------------/ -}

{- /------------------------/
  /  parameters            /
 /------------------------/ -}

pixel_size   = 10  :: Int
grid_size    = 50  :: Int

inital_conditions :: World
inital_conditions =
    ( LCell (0,0)
        ( LCell (0,0)
            ( NCell )
            ( NCell ) )
        ( LCell (0,0)
            ( NCell )
            ( NCell ) )
    , 0 )

stateColor :: State -> Color
stateColor 0 = black
stateColor 1 = white

{- /------------------------/
  /  the World              /
 /------------------------/ -}

-- world
type World      = (Grid,Time)                               -- (world state, current time)

-- grid
type Grid       = LinkedCell                                -- noted by the first LinkedCell
data LinkedCell = LCell Cell LinkedCell LinkedCell          -- (this, right, down)
                | NCell                                     -- null cell

-- cell                
type Cell       = (State, Int)                              -- cell info
type Time       = Int                                       -- last time updated
type State      = Int                                       -- the state of a cell

{- /------------------------/
  /  control               /
 /------------------------/ -}

update :: Float -> World -> World
update dt w = w

{- /------------------------/
  /  render                /
 /------------------------/ -}

renderCell :: Cell -> Int -> Int -> Picture
renderCell (state,_) x y =
    translate sx sy
    $ color (stateColor state)
    $ rectangleSolid 10 10
    where (sx,sy) = (s x,s y)
          s i = intToFloat (i * pixel_size)

intToFloat :: Int -> Float
intToFloat x = (fromInteger $ toInteger x) :: Float

renderGrid :: Grid -> Picture
renderGrid grid =
    let helper :: LinkedCell -> Int -> Int -> Int -> [Picture]
        helper (LCell c r d) 0 x y = thisRow ++ nextRow
            where thisRow = renderCell c x y : helper r 1 (x+1) y
                  nextRow = helper d 0 x (y+1)
        helper (LCell c r d) 1 x y = renderCell c x y : helper r 1 (x+1) y
        helper (NCell)         _ _ _ = []
    in Pictures $ helper grid 0 0 0

render :: World -> Picture
render (g,t) = renderGrid g

{- /------------------------/
  /  input                 /
 /------------------------/ -}

-- handle input events
input :: Event -> World -> World
input (EventKey key key_state modifiers (x,y)) w = w
input (EventMotion (x, y)) w = w
input _ w = w

{- /------------------------/
  /  main                  /
 /------------------------/ -}

window :: Display
window =
    (InWindow
        "Game of Life - Haskell"
        (s,s)
        (10,  10))
    where s = pixel_size * grid_size

background_color :: Color
background_color = black

rate :: Int
rate = 2

main = play window background_color rate inital_conditions render input update
