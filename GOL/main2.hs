import Graphics.Gloss.Interface.Pure.Game
import GOL2

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

cell_size = 20 :: Int

{- /------------------------/
  /  the World              /
 /------------------------/ -}

type World = Grid

inital_conditions = grid

{- /------------------------/
  /  control               /
 /------------------------/ -}

update :: Float -> World -> World
update dt w = updateGrid w

{- /------------------------/
  /  render                /
 /------------------------/ -}

render :: World -> Picture
render w = Pictures $ map helper indecies_flat
    where cs_f = intToFloat cell_size
          helper (x,y) = translate sx sy $ color (cellColor cell) $ rectangleSolid cs_f cs_f
              where cell = getCell w (x,y)
                    sx = (intToFloat $ x * cell_size) - (half_size + half_cell_size)
                    sy = (intToFloat $ y * cell_size) - (half_size + half_cell_size)
                    half_size = (intToFloat $ l * cell_size) / 2.0
                    l = length w
                    half_cell_size = (intToFloat cell_size) / 2.0

intToFloat :: Int -> Float
intToFloat x = (fromInteger $ toInteger x) :: Float

cellColor :: Cell -> Color
cellColor Live = white
cellColor Dead = black

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
    where s = size * cell_size

background_color :: Color
background_color = black

rate :: Int
rate = 10

main = play window background_color rate inital_conditions render input update
