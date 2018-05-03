import Graphics.Gloss

main :: IO ()
main = 
    animate
        (InWindow -- Display
            "Simple Animation"
            (600, 600)
            (20, 20))
        black -- Color
        frame -- (Float -> Picture)

frame :: Float -> Picture
frame time =
    Color white
    $ Scale 120 120
    $ Rotate (time * 2*pi)
    $ Pictures
        [Translate ( 1) ( 1) $ circle 1
        ,Translate (-1) ( 1) $ circle 1
        ,Translate ( 1) (-1) $ circle 1
        ,Translate (-1) (-1) $ circle 1]