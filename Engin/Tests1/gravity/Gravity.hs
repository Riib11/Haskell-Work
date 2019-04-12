import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector

{- /------------------------/
  /  parameters            /
 /------------------------/ -}

window_size  = (600,600) :: (Int,Int)
damping      = 0.5       :: Float
gravity_step = 10        :: Float
zoom_step    = 0.9       :: Float

{- /------------------------/
  /  Particle              /
 /------------------------/ -}

data Particle = Particle
    { mass  :: Float
    , pos   :: Vector
    , vel   :: Vector
    , acc   :: Vector }

getProperties :: Particle -> (Float,Vector,Vector,Vector)
getProperties prt = (mass prt, pos prt, vel prt, acc prt)

newParticle :: Float -> Vector -> Particle
newParticle m p = makeParticle m p (0,0)

makeParticle :: Float -> Vector -> Vector -> Particle
makeParticle m p v = Particle
    { mass=m, pos=p, vel=v, acc=(0,0) }

getColor :: Particle -> Color
getColor prt = white

getRadius :: Particle -> Float
getRadius prt = 5 * (sqrt $ mass prt)

getGravity :: Float -> Particle -> Particle -> Vector
getGravity g prt1 prt2 = mulSV (g*m1*m2/(r^2)) vec
    where r   = magV vec
          vec = p2 - p1
          (m1,p1,_,_) = getProperties prt1
          (m2,p2,_,_) = getProperties prt2

-- exacts gravitational force on both of a pair of particles
applyGravity :: Float -> Float -> Particle -> Particle -> (Particle,Particle)
applyGravity g dt prt1 prt2 = (nprt1, nprt2)
    where nprt1 = applyForce   dt  prt1 fg
          nprt2 = applyForce (-dt) prt2 fg
          fg    = getGravity g prt1 prt2

applyForce :: Float -> Particle -> Vector -> Particle
applyForce dt prt f = Particle
    { mass=m, pos=p, vel=v, acc=na }
        where na = a + (mulSV (dt/m) f)
              (m,p,v,a) = getProperties prt

{- /------------------------/
  /  World                 /
 /------------------------/ -}

data World = World
    { particles :: [Particle]
    , gravity   :: Float
    , zoom     :: Float }

addParticle :: World -> Particle -> World
addParticle w prt = World
    {particles=prt:particles w, gravity=gravity w, zoom=zoom w}

changeGravity :: World -> Float -> World
changeGravity w dg = World
    {particles=particles w, gravity=dg+gravity w, zoom=zoom w}

changeZoom :: World -> Float -> World
changeZoom w ds =
    let ns = ds * zoom w
    in if ns <= 1
        then World {particles=particles w, gravity=gravity w, zoom=ns}
        else w

{- /------------------------/
  /  game control methods  /
 /------------------------/ -}

-- window, display mode
window :: Display
window = InWindow "Gravity" window_size (10,10)


-- window background color
background_color :: Color
background_color = black


-- updates per second
rate :: Int
rate = 80


-- initial world state
inital_conditions :: World
inital_conditions = World
    { particles =
        [ newParticle 5  ( 100, 100)
        , newParticle 5  (-100, 100)
        , newParticle 5  (-50 ,-50 )
        , newParticle 10 ( 20 , 20 )
        , newParticle 1  (-100,-100)
        , newParticle 50 (-20 , 20 )
        , newParticle 3  (-100, 40 )
        , newParticle 5  ( 100, 40 )
        , newParticle 4  ( 50 ,-300)
        , newParticle 7  ( 200, 300) ]
    , gravity = gravity_step
    , zoom   = 1.0 }


-- function to convert world to picture
render :: World -> Picture
render w =
    let prts = particles w
        z    = zoom w
        g    = gravity w
    in Pictures
        [ color white
            $ translate (-100) (-300)
            $ scale 0.5 0.5
            $ text $ "G=" ++ (show g)
        , scale z z
            $ Pictures $ renderParticles prts ]

renderParticles :: [Particle] -> [Picture]
renderParticles [] = []
renderParticles (p:ps) =
    pic : renderParticles ps
        where pic   = color c $ translate x y $ circleSolid r
              (x,y) = pos p
              r     = getRadius p
              c     = getColor p

-- update called each simulation step
update :: Float -> World -> World
update dt w = World
    { particles = updateParticles g dt prts
    , gravity   = g
    , zoom     = s }
        where (prts,g,s) = (particles w,gravity w,zoom w)

-- applies gravity between all particles
updateParticles :: Float -> Float -> [Particle] -> [Particle]
updateParticles g dt []            = []
updateParticles g dt (target:rest) =
    (updateMovement newTarget) : (updateParticles g dt newRest)
        where (newTarget, _, newRest) = helper target rest []
              -- applies gravity all prt pairs with target
              helper :: Particle -> [Particle] -> [Particle] -- target, rest, cumulative newRest
                    -> (Particle,[Particle],[Particle])      -- (newTarget, otherRest, newRest)
              helper t []     nrs = (t, [], nrs)
              helper t (r:rs) nrs = helper nt rs (nr:nrs)
                    where (nt,nr) = applyGravity g dt t r

-- move particle and reset accleration
updateMovement :: Particle -> Particle
updateMovement prt = makeParticle m (p + (mulSV damping v)) (v + a)
    where (m,p,v,a) = getProperties prt


{- /------------------------/
  /  input                 /
 /------------------------/ -}

input :: Event -> World -> World
input (EventKey key Down modifiers (x,y)) w =
    case key of
        (Char 's') -> changeGravity w (-gravity_step)
        (Char 'w') -> changeGravity w ( gravity_step)
        (Char '-') -> changeZoom w (  zoom_step)
        (Char '+') -> changeZoom w (1/zoom_step)
        (Char 'r') -> inital_conditions
        _ -> w
input (EventMotion (x, y)) w = w
input _ w = w

{- /------------------------/
  /  main                  /
 /------------------------/ -}

main = do
    putStrLn $
        "\n" 
        ++ "-------------------------------------------\n" 
        ++ "\n"
        ++ "# Gravity - a simple gravity simulation\n" 
        ++ "\n" 
        ++ "## Controls\n" 
        ++ " s/w : inc/dec gravitational constant\n" 
        ++ " -/+ : zoom out/in\n" 
        ++ "  r  : reset\n" 
        ++ " esc : exit\n" 
        ++ "\n" 
        ++ "-------------------------------------------\n" 
        ++ "\n"
    play window background_color rate inital_conditions render input update
