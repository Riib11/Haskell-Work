import Graphics.Gloss.Interface.Pure.Game

-- parameters

start_position :: Vector
start_position = (0,-200)

gravity :: Vector
gravity = (0.0,5.0)

bounciness :: Vector
bounciness = (0.5,0.8)

worldSize :: (Float,Float)
worldSize = (600,600)

-- A ball has a radius, position, and velocity
data Ball = Ball Float Vector Vector

-- world state
-- keeps track of the ball
type World = Ball

worldSizePixels :: (Int,Int)
worldSizePixels = (floor $ fst worldSize, floor $ snd worldSize)

applyGravity :: Float -> Vector -> Vector
applyGravity dt v =
    (fst v + (dt * fst gravity)
    ,snd v + (dt * snd gravity))

-- main
main = do
    putStrLn $
        "\n" ++
        "-------------------------------------------\n" ++
        "\n" ++
        "# Bounce - a simple bouncing simulation\n" ++
        "\n" ++
        "## Controls\n" ++
        " <arrow keys> : apply force (discrete)\n" ++
        " <r>          : reset\n" ++
        " <esc>        : exit\n" ++
        "\n" ++
        "-------------------------------------------\n" ++
        "\n"
    play window background_color rate inital_conditions render input update


-- display mode (window)
window :: Display
window =
    (InWindow
        "Bounce"            -- window title
        worldSizePixels     -- window size
        (10,10))            -- window position


-- window background color
background_color :: Color
background_color = black


-- updates per second
rate :: Int
rate = 80


-- initial world state
inital_conditions :: World
inital_conditions =
    Ball 20 start_position (0,0)


-- function to convert world to picture
render :: World -> Picture
render (Ball r (x,y) _) =
    Color white
    $ Translate (-x) (-y)
    $ circle r


-- handle input events
input :: Event -> World -> World
input (EventKey key key_state modifiers (x,y)) (Ball r p v) =
    case key of
        (SpecialKey sk) ->
            case sk of
                -- apply directional force
                KeyRight -> Ball r p (v + (-1, 0))
                KeyLeft  -> Ball r p (v + ( 1, 0))
                KeyUp    -> Ball r p (v + ( 0,-1))
                KeyDown  -> Ball r p (v + ( 0, 1))
        (Char c) ->
            case c of
                'r' -> Ball r start_position (0,0) -- reset
input (EventMotion (x, y)) w = w
input _ w = w


-- update called each simulation step
update :: Float -> World -> World
update dt (Ball r p v) =
    applyWalls $ Ball r (p + v) (applyGravity dt v)

-- contain ball position inside
data Bounded = OutLeft | OutRight | OutTop | OutBot | Inbound
applyWalls :: Ball -> Ball
applyWalls (Ball r (x,y) (vx,vy)) =
    (Ball r (nx,ny) (nvx, nvy))
        where
            -- x direction
            (nx,nvx) = case fixDirX of
                OutLeft  -> (-halfWidth + r, -bx * vx)
                OutRight -> ( halfWidth - r, -bx * vx)
                Inbound  -> ( x,          vx)
            -- y direction
            (ny,nvy) = case fixDirY of
                OutTop  -> (-halfHeight + r, -by * vy)
                OutBot  -> ( halfHeight - r, -by * vy)
                Inbound -> ( y,           vy)
            -- calculation
            (fixDirX,fixDirY) =
                (if x < -halfWidth + r  then OutLeft else if x > halfWidth - r  then OutRight else Inbound
                ,if y < -halfHeight + r then OutTop  else if y > halfHeight - r then OutBot   else Inbound)
            (halfHeight,halfWidth) =
                (fst worldSize/2
                ,snd worldSize/2)
            (bx,by) =
                (fst bounciness
                ,snd bounciness)